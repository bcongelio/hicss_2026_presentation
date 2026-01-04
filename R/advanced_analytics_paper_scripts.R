### load packages
library(tidyverse)
library(sf)
library(igraph)
library(mgcv)

######################################
### create grid for spatial analysis
######################################

toledo_bbox <- st_bbox(toledo_bend_shape)

### read in data
tournament_data <- vroom::vroom("./data/toledo_bend_2024_elite.csv") |> 
  filter(!is.na(catch_latitude) & !is.na(catch_longitude))

### convert to sf for spatial filtering
toledo_sf <- tournament_data |> 
  st_as_sf(coords = c("catch_longitude", "catch_latitude"), crs = 4326)

### filter to only catches within lake boundary
valid_catches_sf <- toledo_sf[st_within(toledo_sf, toledo_bend_shape, sparse = FALSE), ]

### extract coordinates back to regular dataframe
valid_coords <- st_coordinates(valid_catches_sf) |> 
  as.data.frame() |> 
  rename(catch_longitude = X, catch_latitude = Y)

### bind coordinates back to the data
tournament_data <- valid_catches_sf |> 
  st_drop_geometry() |>  ### remove sf geometry column
  bind_cols(valid_coords) |>  ### add back lat/long as regular columns
  mutate(valid_coords = TRUE)  ### flag for valid coordinates

### get bounding box from cleaned data
bbox <- tournament_data |> 
  summarize(
    min_lat = min(catch_latitude),
    max_lat = max(catch_latitude),
    min_long = min(catch_longitude),
    max_long = max(catch_longitude))

### create grid function
create_grid <- function(bbox, grid_size = 20) {
  
  lat_seq <- seq(bbox$min_lat, bbox$max_lat, length.out = grid_size + 1)
  long_seq <- seq(bbox$min_long, bbox$max_long, length.out = grid_size + 1)
  
  # create the grid cells
  grid_cells <- expand.grid(
    lat_idx = 1:grid_size,
    long_idx = 1:grid_size) |> 
    mutate(
      cell_id = paste0(lat_idx, "_", long_idx),
      lat_min = lat_seq[lat_idx],
      lat_max = lat_seq[lat_idx + 1],
      long_min = long_seq[long_idx],
      long_max = long_seq[long_idx + 1])
  
  return(grid_cells)
}

### run and create the grid
grid <- create_grid(bbox, grid_size = 20)

### assign catches to grid cells
tournament_data <- tournament_data |> 
  mutate(
    grid_cell = case_when(
      valid_coords ~ {
        
        # for each row find which grid cell it belongs to
        sapply(1:n(), function(i) {
          lat <- catch_latitude[i]
          long <- catch_longitude[i]
          
          cell <- grid |> 
            filter(
              lat >= lat_min & lat < lat_max,
              long >= long_min & long < long_max) |> 
            pull(cell_id)
          
          if(length(cell) == 0) return(NA_character_)
          return(cell[1])
        })
      },
      TRUE ~ NA_character_))


######################################
### calculate location productivity
######################################

location_productivity <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  group_by(grid_cell) |> 
  summarize(
    total_catches = n(),
    keepers = sum(keeper, na.rm = TRUE),
    total_weight = sum(fish_weight[keeper == 1], na.rm = TRUE),
    unique_anglers = n_distinct(angler_id),
    avg_weight = if_else(keepers > 0, total_weight / keepers, 0),
    keeper_ratio = if_else(total_catches > 0, keepers / total_catches, 0))



######################################
### calculate performance by location
######################################
location_by_performance <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  mutate(
    performance_group = case_when(
      finish <= 10 ~ "Top 10",
      finish <= 40 ~ "Middle 11-40",
      TRUE ~ "Bottom 41")) |> 
  group_by(grid_cell, performance_group) |> 
  summarize(
    catches = n(),
    weight = sum(if_else(keeper == 1, fish_weight, 0), na.rm = TRUE),
    .groups = "drop") |> 
# need to pivot wide for calculations
pivot_wider(
  names_from = performance_group,
  values_from = c(catches, weight),
  values_fill = list(catches = 0, weight = 0)) |> 
  janitor::clean_names()


######################################
### calculate temporal value
######################################
temporal_location_value <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  group_by(grid_cell, day) |> 
  summarize(
    segment_catches = n(),
    segment_weight = sum(if_else(keeper == 1, fish_weight, 0), na.rm = TRUE),
    .groups = "drop") |> 
  pivot_wider(
    names_from = day,
    values_from = c(segment_catches, segment_weight),
    values_fill = list(segment_catches = 0, segment_weight = 0))


######################################
### build location networks
######################################
angler_location_data <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  distinct(angler_id, grid_cell) |> 
  group_by(angler_id) |> 
  summarize(
    location_count = n_distinct(grid_cell),
    locations = list(unique(grid_cell)),
    .groups = "drop")

### create edges between locations that share anglers
location_edges <- data.frame(from = character(), to = character())

for (i in 1:nrow(angler_location_data)) {
  
  angler_locs <- angler_location_data$locations[[i]]
  
  if (length(angler_locs) > 1) {
    
    # generate all pairwise combos
    loc_pairs <- combn(angler_locs, 2, simplify = TRUE)
    
    # create temporary df
    temp_edges <- data.frame(
      from = loc_pairs[1, ],
      to = loc_pairs[2, ])
    
    # append to main edge list
    location_edges <- rbind(location_edges, temp_edges)
  }
}

### count edge frequencies for weighting
edge_weights <- location_edges |> 
  group_by(from, to) |> 
  summarize(weight = n(), .groups = "drop")

### create the network
location_network <- graph_from_data_frame(edge_weights, directed = FALSE)

### calculate network betweenness
location_betweenness <- betweenness(location_network, normalized = TRUE)
location_betweenness_df <- data.frame(
  grid_cell = names(location_betweenness),
  betweenness = as.numeric(location_betweenness))

### merge betweenness with productivity data
location_data <- location_productivity |> 
  left_join(location_betweenness_df, by = "grid_cell") |> 
  mutate(betweenness = replace_na(betweenness, 0))

location_data

######################################
### calculate the core metrics
######################################

### PWC (Productive Water Coefficient)
location_data <- location_data |> 
  mutate(
    # normalize everything
    norm_avg_weight = avg_weight / max(avg_weight, na.rm = TRUE),
    norm_keeper_ratio = keeper_ratio / max(keeper_ratio, na.rm = TRUE),
    # calculate PWC
    PWC = betweenness * norm_avg_weight * norm_keeper_ratio)

### CAM (Competitive Advantage Metric)
location_by_performance <- location_by_performance |> 
  mutate(
    # handle missing values, if any
    weight_top_10 = replace_na(weight_top_10, 0),
    weight_bottom_41 = replace_na(weight_bottom_41, 0),
    # avoid dividing by 0
    weight_bottom = pmax(0.001, weight_bottom_41),
    weight_top = pmax(0.001, weight_top_10),
    # calculate CAM
    CAM = (weight_top / sum(weight_top, na.rm = TRUE)) /
      (weight_bottom / sum(weight_bottom, na.rm = TRUE)))

### TVD (Temporal Value Differential)
temporal_location_value <- temporal_location_value |> 
  rowwise() |> 
  mutate(
    # extract weights for days that exist in the tournament
    day_weights = list(c_across(starts_with("segment_weight"))),
    day_weights_clean = list(day_weights[!is.na(day_weights) & day_weights > 0]),
    # calculate MEAN and SD
    mean_weight = if_else(length(day_weights_clean) > 0, 
                          mean(unlist(day_weights_clean), na.rm = TRUE), 
                          0.001),
    sd_weight = if_else(length(day_weights_clean) > 1,
                        sd(unlist(day_weights_clean), na.rm = TRUE), 
                        0),
    # calculate TVD
    TVD = sd_weight / pmax(0.001, mean_weight)) |> 
  select(grid_cell, TVD)


######################################
### ANGLER LEVEL METRICS CALCULATIONS
######################################

### calculate angler-specific location metrics
angler_location_metrics <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  group_by(angler_id, grid_cell) |> 
  summarize(
    total_catches = n(),
    keepers = sum(keeper, na.rm = TRUE),
    total_weight = sum(fish_weight[keeper == 1], na.rm = TRUE),
    avg_weight = if_else(keepers > 0, total_weight / keepers, 0),
    keeper_ratio = if_else(total_catches > 0, keepers / total_catches, 0),
    .groups = "drop")

### join global location data to angler data
angler_location_metrics <- angler_location_metrics |> 
  left_join(select(location_data, grid_cell, betweenness), by = "grid_cell") |> 
  left_join(select(location_by_performance, grid_cell, CAM), by = "grid_cell") |> 
  left_join(select(temporal_location_value, grid_cell, TVD), by = "grid_cell")

#### CALCULATE ANGLER SPECIFIC PWC
angler_location_metrics <- angler_location_metrics |> 
  group_by(angler_id) |> 
  mutate(
    ### normalize within angler's locations
    max_avg_weight = max(avg_weight, na.rm = TRUE),
    max_keeper_ratio = max(keeper_ratio, na.rm = TRUE),
    ### handle 0s and NAs
    norm_avg_weight = if_else(max_avg_weight > 0,
                              avg_weight / max_avg_weight,
                              0),
    norm_keeper_ratio = if_else(max_keeper_ratio > 0,
                              keeper_ratio / max_keeper_ratio,
                              0),
    ### calculate angler-specific PWC
    angler_PWC = betweenness * norm_avg_weight * norm_keeper_ratio) |> 
  ungroup()


### CALCULATE ANGLER-SPECIFIC TEMPORAL CONSISTENCY
angler_temporal <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  group_by(angler_id, name, grid_cell, day) |> 
  summarize(
    daily_catches = n(),
    daily_weight = sum(if_else(keeper == 1, fish_weight, 0), na.rm = TRUE),
    .groups = "drop") |> 
  ### pivot to get days as columns
  pivot_wider(
    id_cols = c(angler_id, name, grid_cell),
    names_from = day,
    values_from = c(daily_catches, daily_weight),
    values_fill = list(daily_catches = 0, daily_weight = 0))

### CALCULATE ANGLER-SPECIFIC TVD
angler_tvd <- angler_temporal |>
  rowwise() |> 
  mutate(
    ### get weights for all days
    day_weights = list(c_across(starts_with("daily_weight_"))),
    day_weights_clean = list(day_weights[day_weights > 0]),
    
    ### calculate mean and SD only if there are multiple days of data
    mean_weight = if_else(length(day_weights_clean) > 0, 
                          mean(unlist(day_weights_clean), na.rm = TRUE), 
                          0.001),
    sd_weight = if_else(length(day_weights_clean) > 1,
                        sd(unlist(day_weights_clean), na.rm = TRUE), 
                        0),
    
    ### calculate TVD (coefficient of variation)
    angler_TVD = sd_weight / pmax(0.001, mean_weight)) |> 
  select(angler_id, name, grid_cell, angler_TVD)

### join tvd to angler metrics
angler_location_metrics <- angler_location_metrics |>
  left_join(select(angler_tvd, angler_id, grid_cell, angler_TVD), 
            by = c("angler_id", "grid_cell"))

### CALCULATE ANGLER-SPECIFIC ILIM
angler_location_metrics <- angler_location_metrics |> 
  group_by(angler_id) |> 
  mutate(
    ### normalize components for this angler
    PWC_norm = (angler_PWC - min(angler_PWC, na.rm = TRUE)) / 
      (max(angler_PWC, na.rm = TRUE) - min(angler_PWC, na.rm = TRUE) + 0.001),
    
    ### use global CAM but normalize per angler
    CAM_norm = (CAM - min(CAM, na.rm = TRUE)) / 
      (max(CAM, na.rm = TRUE) - min(CAM, na.rm = TRUE) + 0.001),
    
    ### use angler-specific TVD but invert (lower is better)
    TVD_norm = 1 - (angler_TVD - min(angler_TVD, na.rm = TRUE)) / 
      (max(angler_TVD, na.rm = TRUE) - min(angler_TVD, na.rm = TRUE) + 0.001),
    
    ### use betweenness as a proxy for SPI, normalize per angler
    SPI_norm = (betweenness - min(betweenness, na.rm = TRUE)) / 
      (max(betweenness, na.rm = TRUE) - min(betweenness, na.rm = TRUE) + 0.001),
    
    ### calculate angler-specific ILIM
    angler_ILIM = (PWC_norm + CAM_norm + TVD_norm + SPI_norm) / 4) |> 
  ungroup()


######################################
### CALCULATE NUMBER OF DAYS FISHED AND DAILY PERFORMANCE
######################################

angler_days <- tournament_data |> 
  group_by(angler_id, name) |> 
  summarize(
    days_fished = n_distinct(day),
    max_day = max(day),
    .groups = "drop")

### calculate daily performance
angler_daily_performance <- tournament_data |>
  group_by(angler_id, name, day) |> 
  summarize(
    daily_catches = n(),
    daily_keepers = sum(keeper == 1, na.rm = TRUE),
    daily_weight = sum(if_else(keeper == 1, fish_weight, 0), na.rm = TRUE),
    .groups = "drop")

### calculate tournament-level metrics with days fished adjustment  
angler_tournament_metrics <- angler_location_metrics |> 
  group_by(angler_id) |> 
  summarize(
    ### total catches across all locations for this angler
    total_catches = sum(total_catches),
    total_keepers = sum(keepers),
    
    ### weight metrics by keepers instead of total catches
    avg_PWC = sum(angler_PWC * keepers, na.rm = TRUE) / sum(keepers, na.rm = TRUE),
    avg_CAM = sum(CAM * keepers, na.rm = TRUE) / sum(keepers, na.rm = TRUE),
    avg_TVD = sum(angler_TVD * keepers, na.rm = TRUE) / sum(keepers, na.rm = TRUE),
    avg_ILIM = sum(angler_ILIM * keepers, na.rm = TRUE) / sum(keepers, na.rm = TRUE),
    
    ### count quality locations used
    high_value_locations = sum(angler_ILIM > 0.75, na.rm = TRUE),
    total_locations = n_distinct(grid_cell),
    
    ### location strategy metrics
    location_diversity = total_locations / sum(keepers, na.rm = TRUE),
    high_value_usage = high_value_locations / total_locations,
    .groups = "drop")

### join with days fished data
angler_tournament_metrics <- angler_tournament_metrics |> 
  left_join(select(angler_days, angler_id, days_fished, max_day), by = "angler_id")

### calculate adjusted metrics that account for tournament cuts
angler_tournament_metrics <- angler_tournament_metrics |> 
  mutate(
    ### safety checks
    days_fished = replace_na(days_fished, 1),
    max_day = replace_na(max_day, 1),
    
    ### daily rates
    daily_keepers = total_keepers / days_fished,
    daily_catches = total_catches / days_fished,
    
    ### calculate cut-adjusted metrics
    ### normalized to full tournament potential
    adj_PWC = avg_PWC * (days_fished / 4),
    adj_CAM = avg_CAM * (days_fished / 4),
    adj_TVD = avg_TVD * (days_fished / 4),
    adj_ILIM = avg_ILIM * (days_fished / 4),
    
    ### daily rate metrics - per-day performance
    PWC_rate = avg_PWC / days_fished,
    CAM_rate = avg_CAM / days_fished,
    TVD_rate = avg_TVD / days_fished,
    ILIM_rate = avg_ILIM / days_fished,
    
    ### cumulative metrics - reward longevity
    cum_PWC = avg_PWC * days_fished,
    cum_CAM = avg_CAM * days_fished,
    cum_TVD = avg_TVD * days_fished,
    cum_ILIM = avg_ILIM * days_fished,
    
    ### compound metrics - balance rate and longevity
    compound_PWC = avg_PWC * sqrt(days_fished),
    compound_CAM = avg_CAM * sqrt(days_fished),
    compound_TVD = avg_TVD * sqrt(days_fished),
    compound_ILIM = avg_ILIM * sqrt(days_fished))
  
  ### join with tournament finish data
tournament_results <- tournament_data |> 
  distinct(angler_id, name, finish) |> 
  arrange(finish)

angler_performance_analysis <- angler_tournament_metrics |> 
  left_join(tournament_results, by = "angler_id") |> 
  arrange(finish)

### created weighted performance index
angler_performance_analysis <- angler_performance_analysis %>%
  mutate(
    ### normalize key compound metrics (0-1 scale)
    norm_compound_PWC = (compound_PWC - min(compound_PWC, na.rm = TRUE)) / 
      (max(compound_PWC, na.rm = TRUE) - min(compound_PWC, na.rm = TRUE) + 0.001),
    
    norm_compound_CAM = (compound_CAM - min(compound_CAM, na.rm = TRUE)) / 
      (max(compound_CAM, na.rm = TRUE) - min(compound_CAM, na.rm = TRUE) + 0.001),
    
    ### TVD is inverted since lower is better for consistency
    norm_compound_TVD = 1 - (compound_TVD - min(compound_TVD, na.rm = TRUE)) / 
      (max(compound_TVD, na.rm = TRUE) - min(compound_TVD, na.rm = TRUE) + 0.001),
    
    norm_compound_ILIM = (compound_ILIM - min(compound_ILIM, na.rm = TRUE)) / 
      (max(compound_ILIM, na.rm = TRUE) - min(compound_ILIM, na.rm = TRUE) + 0.001),
    
    ### create a weighted performance index that emphasizes CAM based on previous analysis
    performance_index = (norm_compound_PWC * 0.2) + 
      (norm_compound_CAM * 0.4) + 
      (norm_compound_TVD * 0.1) + 
      (norm_compound_ILIM * 0.3))

### integrate location metrics
integrated_metrics <- location_data |> 
  left_join(select(location_by_performance, grid_cell, CAM), by = "grid_cell") |> 
  left_join(select(temporal_location_value, grid_cell, TVD), by = "grid_cell") |> 
  ### handle missing values
  mutate(
    CAM = replace_na(CAM, 0),
    TVD = replace_na(TVD, 0),
    ### SPI - use betweenness as a proxy
    SPI = betweenness)


### normalize metrics for ILIM
integrated_metrics <- integrated_metrics %>%
  mutate(
    #### normalize individual metrics (0-1 scale)
    PWC_norm = (PWC - min(PWC, na.rm = TRUE)) / 
      (max(PWC, na.rm = TRUE) - min(PWC, na.rm = TRUE)),
    
    SPI_norm = (SPI - min(SPI, na.rm = TRUE)) / 
      (max(SPI, na.rm = TRUE) - min(SPI, na.rm = TRUE)),
    
    CAM_norm = (CAM - min(CAM, na.rm = TRUE)) / 
      (max(CAM, na.rm = TRUE) - min(CAM, na.rm = TRUE)),
    
    ### For TVD, lower is better, so invert the normalization
    TVD_norm = 1 - (TVD - min(TVD, na.rm = TRUE)) / 
      (max(TVD, na.rm = TRUE) - min(TVD, na.rm = TRUE)),
    
    #### calculate integrated importance
    ILIM = (PWC_norm + SPI_norm + CAM_norm + TVD_norm) / 4)


write.csv(angler_performance_analysis, "./data/final_angler_analysis.csv", row.names = FALSE)

######################################
## RESULTS
######################################

### tournament overview statistics
tournament_overview <- tournament_data |> 
  summarize(
      total_catches = n(),
      total_keepers = sum(keeper == 1, na.rm = TRUE),
      avg_fish_weight = mean(fish_weight, na.rm = TRUE),
      unique_anglers = n_distinct(angler_id),
      spatial_cells_used = n_distinct(grid_cell, na.rm = TRUE))

daily_patterns <- tournament_data |> 
  group_by(day) |> 
  summarize(
    daily_catches = n(),
    daily_keepers = sum(keeper == 1, na.rm = TRUE),
    avg_daily_weight = mean(fish_weight, na.rm = TRUE),
    active_anglers = n_distinct(angler_id),
    .groups = "drop")

### spatial concentration analysis
spatial_concentration <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  group_by(grid_cell) |> 
  summarize(cell_catches = n(), .groups = "drop") |> 
  arrange(desc(cell_catches)) |> 
  mutate(
    cumulative_catches = cumsum(cell_catches),
    cumulative_pct = cumulative_catches / sum(cell_catches),
    cell_rank_pct = row_number() / n())

### performance basesd spatial analysis
performance_spatial <- tournament_data |> 
  filter(!is.na(grid_cell)) |> 
  mutate(
    performance_tier = case_when(
      finish <= 10 ~ "Elite (1-10)",
      finish <= 30 ~ "High (11-30)", 
      finish <= 60 ~ "Middle (31-60)",
      TRUE ~ "Lower (61+)")) |> 
  group_by(performance_tier, angler_id) |> 
  summarize(
    locations_visited = n_distinct(grid_cell),
    total_catches = n(),
    total_keepers = sum(keeper == 1, na.rm = TRUE),
    total_weight = sum(fish_weight * keeper, na.rm = TRUE),
    spatial_efficiency = total_keepers / locations_visited,
    weight_per_location = total_weight / locations_visited,
    .groups = "drop") |> 
  group_by(performance_tier) |> 
  summarize(
    avg_locations = mean(locations_visited),
    avg_spatial_efficiency = mean(spatial_efficiency),
    avg_weight_per_location = mean(weight_per_location),
    .groups = "drop")

# PWC analysis and top locations
pwc_analysis <- integrated_metrics %>%
  arrange(desc(PWC)) %>%
  filter(PWC > 0) %>%
  mutate(
    pwc_quartile = ntile(PWC, 4),
    high_value = PWC > 0.5
  )

# PWC correlation with angler success
pwc_success_correlation <- angler_performance_analysis %>%
  select(name, finish, compound_PWC, total_keepers, days_fished) %>%
  filter(!is.na(compound_PWC)) %>%
  mutate(success_rank = 104 - finish) # Con

ggplot(data = angler_performance_analysis, aes(x = adj_PWC, y = finish)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_reverse(breaks = scales::pretty_breaks()) +
  scale_x_continuous(breaks = scales::pretty_breaks())


cam_analysis <- integrated_metrics %>% 
  filter(!is.na(CAM), CAM > 0) %>%
  arrange(desc(CAM)) %>%
  mutate(
    elite_advantage = CAM > 2.0,
    cam_category = case_when(
      CAM > 2.0 ~ "High Elite Advantage",
      CAM > 1.2 ~ "Moderate Elite Advantage", 
      CAM > 0.8 ~ "Neutral",
      TRUE ~ "Lower Performer Advantage"
    )
  )

# Elite advantage locations
elite_advantage_locations <- cam_analysis %>%
  filter(elite_advantage) %>%
  select(grid_cell, CAM, total_catches, avg_weight) %>%
  arrange(desc(CAM))

# CAM effectiveness analysis
cam_effectiveness <- angler_performance_analysis %>%
  select(name, finish, compound_CAM, performance_index) %>%
  arrange(-compound_CAM)


# TVD consistency analysis
tvd_analysis <- integrated_metrics %>%
  filter(!is.na(TVD)) %>%
  mutate(
    consistency_level = case_when(
      TVD < 0.30 ~ "Highly Consistent",
      TVD < 0.60 ~ "Moderately Consistent",
      TVD < 1.00 ~ "Variable",
      TRUE ~ "Highly Variable"
    )
  ) %>%
  arrange(TVD)

# Most consistent locations
consistent_locations <- tvd_analysis %>%
  filter(consistency_level == "Highly Consistent") %>%
  select(grid_cell, TVD, avg_weight, total_catches) %>%
  arrange(TVD)

# Angler temporal consistency correlation
angler_temporal_performance <- angler_performance_analysis %>%
  select(name, finish, avg_TVD, days_fished, total_keepers) %>%
  filter(days_fished >= 2) %>% # Only include anglers who fished multiple days
  arrange(avg_TVD)


# ILIM comprehensive analysis
ilim_analysis <- integrated_metrics %>%
  filter(!is.na(ILIM)) %>%
  arrange(desc(ILIM)) %>%
  mutate(
    strategic_value = case_when(
      ILIM > 0.80 ~ "Exceptional",
      ILIM > 0.60 ~ "High Value",
      ILIM > 0.40 ~ "Moderate Value",
      TRUE ~ "Limited Value"
    )
  )

# Strategic locations identification
strategic_locations <- ilim_analysis %>%
  filter(strategic_value %in% c("Exceptional", "High Value")) %>%
  select(grid_cell, ILIM, PWC, CAM, TVD, betweenness, total_catches, avg_weight)

# Tournament success correlation with ILIM utilization
ilim_success_analysis <- angler_performance_analysis %>%
  select(name, finish, compound_ILIM, performance_index, days_fished) %>%
  arrange(desc(compound_ILIM)) %>%
  mutate(
    tournament_success = case_when(
      finish <= 10 ~ "Elite",
      finish <= 30 ~ "High Performing", 
      finish <= 50 ~ "Competitive",
      TRUE ~ "Developing"
    )
  )


# Metric correlation analysis
metric_correlations <- integrated_metrics %>%
  select(PWC, CAM, TVD, ILIM, betweenness, avg_weight, total_catches) %>%
  filter(complete.cases(.)) %>%
  cor() %>%
  round(3)

# Convergent validity analysis
convergent_analysis <- integrated_metrics %>%
  filter(!is.na(ILIM)) %>%
  mutate(ilim_quartile = ntile(ILIM, 4)) %>%
  group_by(ilim_quartile) %>%
  summarize(
    avg_PWC = mean(PWC, na.rm = TRUE),
    avg_CAM = mean(CAM, na.rm = TRUE), 
    avg_TVD = mean(TVD, na.rm = TRUE),
    avg_betweenness = mean(betweenness, na.rm = TRUE),
    locations_count = n(),
    .groups = "drop"
  )



prediction_validation <- angler_performance_analysis %>%
  select(name, finish, performance_index, compound_ILIM, compound_CAM, compound_PWC) %>%
  arrange(desc(performance_index)) %>%
  mutate(
    predicted_success_tier = row_number(),
    actual_success_tier = case_when(
      finish <= 10 ~ 1,
      finish <= 25 ~ 2,
      finish <= 50 ~ 3,
      TRUE ~ 4
    ),
    predicted_success_tier = case_when(
      predicted_success_tier <= 10 ~ 1,
      predicted_success_tier <= 25 ~ 2, 
      predicted_success_tier <= 50 ~ 3,
      TRUE ~ 4
    )
  )

# Calculate prediction accuracy
prediction_accuracy <- prediction_validation %>%
  summarize(
    exact_tier_matches = sum(actual_success_tier == predicted_success_tier),
    within_one_tier = sum(abs(actual_success_tier - predicted_success_tier) <= 1),
    total_anglers = n(),
    exact_accuracy = exact_tier_matches / total_anglers,
    within_one_accuracy = within_one_tier / total_anglers
  )



ggplot(data = angler_performance_analysis, aes(x = compound_ILIM, y = performance_index)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.8) +
  geom_point(size = 2.5, color = "#0039A6") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal() +
  labs(x = "ILIM (Integrated Location Importance Metric)",
       y = "Performance Index")

 ggsave("angler_performance_ilim_analysis.png", dpi = 600) 
 