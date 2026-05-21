mn_dp_SRs_24_26 <- soc_read(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.json",
  query = soc_query(
    where = "borough = 'MANHATTAN'
             AND created_date >= '2024-01-01T00:00:00'
             AND ((descriptor = 'Dog Waste') OR (descriptor = 'Animal Waste' AND descriptor_2 = 'Dog'))
    "
  )
)

mn_centerlines <- soc_read(
  "https://data.cityofnewyork.us/City-Government/Centerline/inkn-q76z.json",
  query = soc_query(
    where = "boroughcode = '1'"
  )
) 

mn_dp_SRs_24_26 <- mn_dp_SRs_24_26 |> 
  st_join(
    mn_centerlines |> select(objectid),
    join = st_nearest_feature
  )

waste_bag_dispensers <- soc_read(
  "https://data.cityofnewyork.us/Recreation/Canine-Waste-Dispensers/5npv-j6gn.json",
  query = soc_query(
    where = "borough = 'M'"
  )
)

top_blocks_manhattan <- mn_dp_SRs_24_26 |> 
  group_by(community_board, objectid) |> 
  summarise(n_dog_poop_SRs = n()) |> 
  arrange(desc(n_dog_poop_SRs)) |> 
  slice(1:3) |> 
  filter(!(community_board %in% c("64 MANHATTAN", "Unspecified MANHATTAN", "08 BRONX")))

top_blocks_manhattan_counts <- top_blocks_manhattan |> 
  st_drop_geometry() |> 
  filter(community_board != "12 MANHATTAN") |> 
  pull(n_dog_poop_SRs)

mn_litter_basket <- soc_read(
  "https://data.cityofnewyork.us/Environment/DSNY-Litter-Basket-Inventory/8znf-7b2c.json",
  query = soc_query(
    where = paste0(
      "within_polygon(point, '",
      mn_wkt,
      "')"
    )
  )
)

manhattan_comDist <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Community_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson") |> 
  st_make_valid() |> 
  filter(BoroCD < 200) |> 
  mutate(community_board = paste0(
    str_sub(as.character(BoroCD), -2),
    " MANHATTAN"
  ))

mn_wkt <- st_as_text(st_geometry(st_union(manhattan_comDist))[[1]])

mn_litter_basket <- soc_read(
  "https://data.cityofnewyork.us/Environment/DSNY-Litter-Basket-Inventory/8znf-7b2c.json",
  query = soc_query(
    where = paste0(
      "within_polygon(point, '",
      mn_wkt,
      "')"
    )
  )
)

mn_pops <- soc_read(
  "https://data.cityofnewyork.us/City-Government/Privately-Owned-Public-Spaces-POPS-/rvih-nhyn.json",
  query = soc_query(
    where = "borough_name = 'Manhattan'"
  )
)

mn_dog_runs <- soc_read(
  "https://data.cityofnewyork.us/Recreation/Dog-Runs/hxx3-bwgv.json",
  query = soc_query(
    where = paste0(
      "within_polygon(the_geom, '",
      mn_wkt,
      "')"
    )
  )
)

### Map ###
maplibre(style = carto_style("positron")) |> 
  fit_bounds(manhattan_comDist) |> 
  add_line_layer(
    id = "Manhattan Community Districts",
    source = manhattan_comDist,
    line_color = "#888888",
    line_width = 1
  ) |> 
  add_fill_layer(
    id = "Dog Runs",
    source = mn_dog_runs,
    fill_color = "#ff83fb"
  ) |> 
  add_line_layer(
    id = "Top Blocks",
    source = mn_centerlines |> 
      filter(objectid %in% top_blocks_manhattan$objectid),
    line_color = "#b30000",
    line_width = 8
  ) |> 
  add_circle_layer(
    id = "pops",
    source = mn_pops,
    circle_color = "orange",
    circle_radius = 5,
    visibility = "none"
  ) |> 
  add_circle_layer(
    id = "Litter Baskets",
    source = mn_litter_basket,
    circle_color = "lightgreen",
    circle_radius = 5,
    visibility = "none"
    #min_zoom = 16
  )|> 
  add_circle_layer(
    id = "Service Requests",
    source = mn_dp_SRs_24_26 |> 
      filter(objectid %in% top_blocks_manhattan$objectid),
    circle_color = "#444444",
    circle_stroke_color = "#666666",
    circle_radius = 10,
    circle_stroke_width = 2,
    cluster_options = cluster_options(
      max_zoom = 14.5
    )
  ) |> 
  add_circle_layer(
    id = "Canine Waste Bag Dispensers",
    source = waste_bag_dispensers,
    circle_color = "green",
    circle_radius = 5
  ) |> 
  add_layers_control(
    layers = list(
      "Litter Baskets" = "Litter Baskets",
      "POPs" = "pops"
    ),
    position = "top-right"
  ) |> 
  add_categorical_legend(
    legend_title = "Layers",
    values = c("Canine Waste Bag Dispensers",
               "Litter Baskets",
               "POPs", "Dog Runs"),
    colors = c("green", "lightgreen", "orange", "#ff83fb"),
    patch_shape = "circle",
    style = list(
      background_opacity = 0.8
    )
  )

