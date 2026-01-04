library(sf)
library(rnaturalearth)
library(extrafont)
library(ggspatial)
library(leaflet)
library(htmlwidgets)
library(viridis)


### getting toledo bend shapefile from naturalearth
ne_download(scale = 10, type = "lakes",destdir = "./maps", category = "physical", load = FALSE)
lakes <- ne_load(scale = 10, type = "lakes", destdir = "./maps", returnclass = "sf")

toledo_bend_shape <- lakes |> 
  filter(name == "Toledo Bend Reservoir")

### getting bounding box of toledo bend
toledo_bbox <- st_bbox(toledo_bend_shape)

### read in data
toledo_bend_2024_elite <- vroom::vroom("./data/toledo_bend_2024_elite.csv")

### catch coordinates to sf
toledo_sf <- toledo_bend_2024_elite |> 
  filter(!is.na(catch_latitude) & !is.na(catch_longitude)) |> 
  st_as_sf(coords = c("catch_longitude", "catch_latitude"), crs = 4326)

### making sure bad GPS results don't show; only catches within bounding box of lake
catches_within <- toledo_sf[st_within(toledo_sf, toledo_bend_shape, sparse = FALSE), ]


###############
## interactive map
## leaflet
################

core_leaflet <- function(catches_within, toledo_bend_shape) {
  
  ### color palette for tournament days
  day_pal <- colorFactor(
    palette = viridis(4, option = "plasma"),
    domain = catches_within$day)
  
  ### getting bounding box
  bbox <- st_bbox(catches_within)
  
  ### create the map
  map <- leaflet() |> 
    ### base map tiles
    addProviderTiles(providers$CartoDB.Voyager,
                     options = tileOptions(detectRetina = TRUE)) |>

    # Add lake boundary
    addPolygons(
      data = toledo_bend_shape,
      fillColor = "#9ecae1",
      fillOpacity = 0.4,
      color = "#1a5490",
      weight = 2,
      label = "Toledo Bend Reservoir") |> 
    
    # Add catch points
    addCircleMarkers(
      data = catches_within,
      radius = 5,
      fillColor = ~day_pal(day),
      fillOpacity = 0.7,
      color = "white",
      weight = 1,
      popup = ~paste0(
        "<b>Day ", day, "</b><br>",
        "Angler: ", name, "<br>",
        "Weight: ", fish_weight, " lbs<br>",
        "Time: ", catch_time, "<br>",
        "Keeper: ", ifelse(keeper == 1, "Yes", "No")
      ),
      label = ~paste("Day", day, "-", fish_weight, "lbs")
    ) %>%
    
    # Add legend
    addLegend(
      position = "topright",
      pal = day_pal,
      values = catches_within$day,
      title = "Tournament Day",
      labFormat = labelFormat(prefix = "Day ")
    ) %>%
    
    # Add scale bar
    addScaleBar(position = "bottomleft") |> 
    
    fitBounds(
      lng1 = unname(bbox["xmin"]),
      lat1 = unname(bbox["ymin"]),
      lng2 = unname(bbox["xmax"]),
      lat2 = unname(bbox["ymax"]),
      options = list(padding = c(5, 5))
    )
  
  return(map)
}

### build the map
basic_map <- core_leaflet(catches_within, toledo_bend_shape)

### save for embedding
saveWidget(basic_map, file = "./data_viz/basic_toledo_bend_map.html", selfcontained = TRUE)

###############
## network map
################

## extracting the node positions from the grid cells
node_positions <- grid |> 
  mutate(
    lat_center = (lat_min + lat_max) / 2,
    long_center = (long_min + long_max) / 2,
    cell_id = paste0(lat_idx, "_", long_idx)) |> 
  select(cell_id, lat_center, long_center)

## join with network metrics and productivity
network_nodes <- node_positions |> 
  left_join(location_data, by = c("cell_id" = "grid_cell")) |> 
  filter(!is.na(total_catches))  ### only keep cells with activity

### prepare edge coordinates for plotting
network_edges_coords <- edge_weights |> 
  left_join(node_positions, by = c("from" = "cell_id")) |> 
  rename(from_lat = lat_center, from_long = long_center) |> 
  left_join(node_positions, by = c("to" = "cell_id")) |> 
  rename(to_lat = lat_center, to_long = long_center)

#### cereate the viz

### creating zoom bound for better viz
zoom_bounds <- list(
  lat_min = 31.15,
  lat_max = 31.45,
  long_min = -93.85,
  long_max = -93.65)

## filter network nodes to zoom area
network_nodes_zoom <- network_nodes |> 
  filter(
    lat_center >= zoom_bounds$lat_min,
    lat_center <= zoom_bounds$lat_max,
    long_center >= zoom_bounds$long_min,
    long_center <= zoom_bounds$long_max)

# Filter edges where BOTH endpoints are in zoom region
network_edges_zoom <- network_edges_coords |> 
  filter(
    from_lat >= zoom_bounds$lat_min,
    from_lat <= zoom_bounds$lat_max,
    from_long >= zoom_bounds$long_min,
    from_long <= zoom_bounds$long_max,
    to_lat >= zoom_bounds$lat_min,
    to_lat <= zoom_bounds$lat_max,
    to_long >= zoom_bounds$long_min,
    to_long <= zoom_bounds$long_max)

# Clip lake shapefile to zoom region
lake_zoom <- toledo_bend_shape |> 
  st_crop(xmin = zoom_bounds$long_min,
          xmax = zoom_bounds$long_max,
          ymin = zoom_bounds$lat_min,
          ymax = zoom_bounds$lat_max)



network_map <- ggplot() +
  geom_sf(data = lake_zoom, fill = "lightblue", color = "steelblue", alpha = 0.08) +
  ### network edges (connections between locations)
  geom_segment(data = network_edges_zoom, aes(x = from_long, y = from_lat, 
        xend = to_long, yend = to_lat,
        alpha = weight), color = "red", size = 0.5) +
  ### Network nodes (grid cells with catches)
  # Styling
  scale_size_continuous(
    name = "Network\nCentrality",
    range = c(2, 10)) +
  
  scale_fill_viridis_c(
    name = "Total Weight\n(lbs)",
    option = "plasma") +
  
  scale_alpha_continuous(
    name = "Shared\nAnglers",
    range = c(0.1, 0.6)) +
  coord_sf() +
  
  coord_sf(xlim = c(zoom_bounds$long_min, zoom_bounds$long_max),
           ylim = c(zoom_bounds$lat_min, zoom_bounds$lat_max)) +
  
  theme_minimal() +
  theme(
    legend.title = element_text(family = "Roboto Condensed", size = 10),
    legend.text = element_text(family = "Roboto Condensed", size = 8),
    panel.grid = element_line(color = "gray90", size = 0.2)) +
  labs(
    x = "Longitude",
    y = "Latitude")
  
  ggsave("./images/network_map.png", dpi = 400)
  