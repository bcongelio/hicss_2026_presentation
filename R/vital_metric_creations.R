
#################################
## productiviy-weighted centrality (PWC)
## this metric extends traditional betweenees centrality by incorporating catch outcomes
#################################

### calculate productivity metrics for each location
location_productivity <- toledo_locations |> 
  group_by(location_id) |> 
  summarize(
    total_weight = sum(fish_weight),
    catches = n(),
    keepers = sum(keeper),
    avg_weight = mean(fish_weight),
    keeper_ratio = keepers / catches,
    .groups = "drop")

### calculate betweenness centrality
location_betweenness <- betweenness(location_network, normalized = TRUE)

### productivity-weighted centrality
PWC <- location_betweenness * (location_productivity$avg_weight / max(location_productivity$avg_weight, na.rm=TRUE)) * 
  (pmax(0.001, location_productivity$keeper_ratio) / max(location_productivity$keeper_ratio, na.rm=TRUE))


#################################
## strategic positioning index (SPI)
## this novel metric quantifies a location's strategic values based on
## access to alternative productive areas
#################################

### calculate distance matrix between all locations
location_coords <- location_summary |> 
  select(location_id, avg_lat, avg_lon) |> 
  as.matrix()

location_distances <- as.matrix(dist(location_coords[,c("avg_lat", "avg_lon")]))

### calculate time-discounted accessibility
accessibility_matrix <- exp(-location_distances/mean(location_distances))

### strategic positioning index
SPI <- apply(accessibility_matrix * matrix(location_productivity$avg_weight,
                                           nrow = nrow(accessibility_matrix),
                                           ncol = ncol(accessibility_matrix)),
                                           1, sum)

#################################
## competitive advantage metric (CAM)
## captures differential location value acreoss performance strata
#################################

location_by_performance <- toledo_locations |> 
  left_join(angler_summary |> 
              select(angler_id, performance_group), by = "angler_id") |> 
  group_by(location_id, performance_group) |> 
  summarize(
    group_catches = n(),
    group_weight = sum(fish_weight),
    group_keepers = sum(keeper),
    .groups = "drop") |> 
  pivot_wider(
    id_cols = location_id,
    names_from = performance_group,
    values_from = c(group_catches, group_weight, group_keepers),
    values_fill = 0) |> 
  janitor::clean_names()

### calculate differential value
denom_weights <- pmax(0.001, location_by_performance$group_weight_bottom_41)
CAM <- (location_by_performance$group_catches_top_10  / sum(location_by_performance$group_weight_top_10, na.rm = TRUE)) /
  (denom_weights / sum(denom_weights, na.rm = TRUE))

#################################
## temporal value dynamics (TVD)
## captures dynamic value shifts throughout tournament progress
#################################

### calculate temporal value changes
temporal_location_value <- toledo_locations |> 
  mutate(tournament_segment = paste0("Day ", day, "_",
                                     cut(hour(catch_time),
                                         breaks = c(0, 10, 12, 15, 24),
                                         labels = c("Early", "Mid", "Late", "Evening")))) |> 
  group_by(location_id, tournament_segment) |>
  summarize(
    segment_weight = sum(fish_weight),
    segment_catches = n(),
    .groups = "drop") |> 
  pivot_wider(
    id_cols = location_id,
    names_from = tournament_segment,
    values_from = c(segment_weight, segment_catches),
    values_fill = 0) |> 
  janitor::clean_names()

### to handle zero mean issues
segment_means <- apply(temporal_location_value |> 
                         select(contains("segment_weight")) |>
                         as.matrix(), 1, function(x) max(0.001, mean(x, na.rm = TRUE)))

### calculate variation in temporal value
TVD <- apply(temporal_location_value |> 
               select(contains("segment_weight")) |> 
               as.matrix(), 1, sd, na.rm = TRUE) / segment_means

#################################
## integrated location importance metric (ILIM)
## integrated metric incorporating everything above
#################################


### normalize individual metrics
PWC_norm <- (PWC - min(PWC, na.rm = TRUE)) / (max(PWC, na.rm = TRUE) - min(PWC, na.rm = TRUE))
CAM_norm <- (CAM - min(CAM, na.rm = TRUE)) / (max(CAM, na.rm = TRUE) - min(CAM, na.rm = TRUE))

### use normalized individual metrics to calculate ILIM
ILIM <- (PWC_norm + SPI_norm + CAM_norm) / 3

### assign this to location network
V(location_network)$importance <- ILIM

importance_validation <- data.frame(
  location_id = as.numeric(V(location_network)$name),
  importance = ILIM) |> 
  left_join(location_productivity, by = "location_id")

### statistical validation
importance_model <- lm(total_weight ~ importance, data = importance_validation)
summary(importance_model)

### cross-validation with performance data
cv_importance <- toledo_locations |> 
  left_join(data.frame(location_id = as.numeric(V(location_network)$name),
                       importance = ILIM),
            by = "location_id") |> 
  group_by(angler_id) |> 
  summarize(
    avg_importance = weighted.mean(importance, fish_weight),
    .groups = "drop") |> 
  left_join(angler_summary |> 
              select(angler_id, finish), by = "angler_id")

### visualize relationship with performance
ggplot(cv_importance, aes(x = avg_importance, y = finish)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_reverse() +
  labs(
    title = "Relationship Between Location Importance Utilization and Tournament Finish",
    x = "Weighted Average Location Importance",
    y = "Tournament Finish Position") +
  theme_minimal()
