#### load packages
library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(igraph)
library(ggraph)
library(extrafont)

### load the dataset
toledo_data <- vroom::vroom("./data/toledo_bend_2024_elite.csv")

### transform the temporal variables
toledo_clean <- toledo_data |> 
  mutate(
    date = as.Date(date),
    catch_datetime = as.POSIXct(paste(date, catch_time), format = "%Y-%m-%d %H:%M:%S"),
    catch_id = row_number()) |> 
  filter(catch_longitude != 0 | catch_latitude != 0)

################################
## performance categorization framework
################################

angler_summary <- toledo_clean |> 
  group_by(angler_id, name) |> 
  summarize(
    total_weight = sum(fish_weight),
    avg_weight = mean(fish_weight),
    total_keepers = sum(keeper),
    catch_count = n(),
    keeper_ratio = total_keepers / catch_count,
    avg_dist_traveled = mean(dist_to_catch, na.rm = TRUE),
    .groups = "drop") |> 
  left_join(toledo_clean |> 
              select(angler_id, finish) |> 
              distinct(),
            by = "angler_id") |> 
  mutate(
    performance_group = case_when(
      finish <= 10 ~ "Top 10",
      finish <= 20 ~ "Top 11-20",
      finish <= 40 ~ "Middle 21-40",
      TRUE ~ "Bottom 41+"))

################################
## spatial distribution analysis
################################

### convert to sf format
toledo_spatial <- toledo_clean |> 
  st_as_sf(coords = c("catch_longitude", "catch_latitude"), crs = 4326)

ggplot() +
  geom_sf(data = toledo_special, aes(color = fish_weight),
          size = 1, alpha = 0.7) +
  scale_color_viridis_c() +
  labs(
    title = "Spatial Distribution of Catches at Toledo Bend",
    subtitle = "2024 Bassmaster Elite Series Event",
    color = "Fish Weight (lbs)") +
  theme_minimal()

################################
## location cluster identification
################################

### clustering protocol
coords <- st_coordinates(toledo_spatial)
coords_dist <- dist(coords)
hc <- hclust(coords_dist, method = "complete")
location_clusters <- cutree(hc, h = 0.005) ### distance threshold for location definition

### append the new cluster information
toledo_locations <- toledo_clean |> 
  mutate(location_id = location_clusters)


################################
## network construction framework
################################

### location node attributes
location_summary <- toledo_locations |> 
  group_by(location_id) |> 
  summarize(
    catch_count = n(),
    unique_anglers = n_distinct(angler_id),
    avg_weight = mean(fish_weight),
    keeper_ratio = sum(keeper) / catch_count,
    avg_lat = mean(catch_latitude),
    avg_lon = mean(catch_longitude),
    .groups = "drop") |> 
  mutate(
    centrality = unique_anglers / n_distinct(toledo_locations$angler_id))

### edge location based on angler transitions
location_transitions <- toledo_locations |> 
  arrange(angler_id, day, catch_datetime) |> 
  group_by(angler_id, day) |> 
  mutate(
    next_location = lead(location_id),
    transition_time = as.numeric(difftime(lead(catch_datetime), catch_datetime, units = "mins"))) |> 
  filter(!is.na(next_location) & location_id != next_location) |> 
  ungroup()

### aggregate transitions into network edges
location_edges <- location_transitions |> 
  group_by(location_id, next_location) |> 
  summarize(
    weight = n(), ### transition frequency
    avg_time = mean(transition_time, na.rm = TRUE),
    .groups = "drop")

### network constructions
location_network <- graph_from_data_frame(
  d = location_edges,
  vertices = location_summary |> 
    select(id = location_id, everything()),
  directed = TRUE)

################################
## network visualizations approach
################################

### calculate centrality metrics
V(location_network)$degree <- degree(location_network)
V(location_network)$betweenness <- betweenness(location_network)
V(location_network)$pagerank <- page_rank(location_network)$vector

### network visualizations
### this is neat but needs a lot of work to clean it up
set.seed(123) ### reproducibility

ggraph(location_network, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = 1/avg_time),
                 arrow = arrow(length = unit(.5, "mm")),
                 end_cap = circle(3, "mm")) +
  geom_node_point(aes(size = catch_count, color = keeper_ratio)) +
  scale_color_viridis() +
  labs(
    title = "Fishing Location Network",
    subtitle = "2024 Bassmaster Elite Series at Toledo Bend",
    size = "Number of Catches",
    color = "Keeper Ratio",
    edge_width = "Transition Frequency",
    edge_alpha = "Transition Speed") +
  theme_graph()

################################
## comparative network analysis by performance group
################################

### angler specific network metrics
angler_network_metrics <- toledo_locations |> 
  arrange(angler_id, day, catch_datetime) |> 
  group_by(angler_id, day) |> 
  mutate(
    next_location = lead(location_id),
    transition_time = as.numeric(difftime(lead(catch_datetime), catch_datetime, units = "mins"))) |> 
  filter(!is.na(next_location)) |> 
  group_by(angler_id) |> 
  summarize(
    unique_locations = n_distinct(location_id),
    total_transitions = sum(location_id != next_location, na.rm = TRUE),
    avg_transition_time = mean(transition_time[location_id != next_location], na.rm = TRUE),
    spatial_coverage = sd(c(catch_latitude, catch_longitude)),
    revisit_ratio = 1 - (unique_locations / n()),
    .groups = "drop")

### performance integration
angler_performance_network <- angler_network_metrics |> 
  left_join(angler_summary |> 
              select(angler_id, finish, performance_group), by = "angler_id")

### relationship visualization
ggplot(angler_performance_network, aes(x = unique_locations,
                                       y = finish,
                                       color = performance_group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship Between Location Diversity and Tournament Finish",
    x = "Number of Unique Fishing Locations",
    y = "Tournament Finish Position") +
  scale_y_reverse() +
  theme_minimal()

################################
## temporal evolution analysis
################################

### temporal segmentation
time_segments <- toledo_clean |> 
  mutate(
    time_segment = floor(hour(catch_datetime) / 2),
    segment_label = paste0("Day ", day, ": ",
                           time_segment * 2, "-", (time_segment * 2) + 2, "hrs"))

### performance-based temporal patterns
angler_temporal_strategies <- time_segments |> 
  left_join(angler_summary |> 
              select(angler_id, performance_group), by = "angler_id") |> 
  arrange(angler_id, day, catch_datetime) |> 
  group_by(angler_id, day) |> 
  mutate(
    cumulative_dist = cumsum(replace_na(dist_to_catch, 0))) |> 
  group_by(angler_id, day, time_segment, performance_group) |> 
  summarize(
    catches_in_segment = n(),
    avg_weight_in_segment = mean(fish_weight),
    distance_in_segment = sum(dist_to_catch, na.rm = TRUE),
    .groups = "drop")

################################
## temporal strategy visualization
################################
ggplot(angler_temporal_strategies, aes(x = time_segment,
                                       y = distance_in_segment, 
           color = performance_group, group = interaction(angler_id, day))) +
  geom_smooth(aes(group = performance_group), se = TRUE) +
  facet_wrap(~day) +
  labs(
    title = "Movement Strategy Evolution by Performance Group",
    x = "Time Segment (2-hour blocks)",
    y = "Distance Traveled in Segment") +
  theme_minimal()
