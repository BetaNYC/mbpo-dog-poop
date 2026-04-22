library(flextable)
library(sf)
library(tidyverse)
library(socratadata)
library(lubridate)
library(ggrepel)
library(mapview)


readRenviron(".Renviron")

dog_poop_complaints <- soc_read(
  "https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2020-to-Present/erm2-nwe9/about_data",
  query = soc_query(
    where = "borough = 'MANHATTAN' AND ((descriptor = 'Dog Waste') OR (descriptor = 'Animal Waste' AND descriptor_2 = 'Dog'))"
  )
)


monthly_dog_poop_complaints <-  dog_poop_complaints |> 
  st_drop_geometry() |> 
  group_by(yearMonth = paste0(
    year(created_date),
                  "-",
                  str_split_i(date(created_date),
                              "-",
                              2))) |> 
  summarize(monthly_complaints = n()) |> 
  ungroup() |> 
  # Pin to first of month for plotting
  mutate(date = as.Date(paste0(yearMonth, "-01")))


ggplot(monthly_dog_poop_complaints,
       aes(x = date,
           y = monthly_complaints)) +
  geom_point()+
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  )

# 2026 by board
dog_poop_complaints |> 
  st_drop_geometry() |> 
  filter(year(created_date)>2025) |> 
  group_by(community_board) |> 
  summarize(complaints_2026 = n())


# Convert quarter mile to decimal degrees
library(measurements)
conv_unit(0.25, "mi", "m")

library(mapview)

mapview(st_buffer(mn_link_withScreens,
                  dist = 400))


manhattan_zcta <- read_sf("https://services2.arcgis.com/FiaPA4ga0iQKduv3/ArcGIS/rest/services/Census_ZIP_Code_Tabulation_Areas_2010_v1/FeatureServer/0/query?where=1%3D1&geometry=-74.0479%2C40.6829%2C-73.9067%2C40.8820&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outFields=*&outSR=4326&f=geojson")
mapview(manhattan_zcta |> 
          filter(str_starts(ZCTA5, "10") & !str_starts(ZCTA5, "104")))

mapview(comDist)


manhattan_zcta |> 
  filter(!(dog_poop_SRs_26 == 0 & is.na(n_dog_licenses)))

mn_complaints_byMonthYear <- soc_read(
  "https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2020-to-Present/erm2-nwe9/about_data",
  query = soc_query(
    select = "date_trunc_ym(created_date) AS year_month, count(*) AS monthly_complaints",
    where  = "borough = 'MANHATTAN'",
    group  = "date_trunc_ym(created_date)",
    order  = "year_month"
  )
)

mn_SRs_2026 <- soc_read(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.json",
  query = soc_query(
    where = "borough = 'MANHATTAN'
             AND created_date >= '2026-01-01T00:00:00'
             AND created_date <  '2027-01-01T00:00:00'"
  )
)

mn_SRs_20_26 <- soc_read(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.json",
  query = soc_query(
    select = "community_board, count(*) as count",
    where = "borough = 'MANHATTAN'",
    group_by = "community_board"
  )
)

mn_dp_SRs_20_26 <- soc_read(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.json",
  query = soc_query(
    select = "community_board, count(*) as dp_count",
    where = "borough = 'MANHATTAN' AND ((descriptor = 'Dog Waste') OR (descriptor = 'Animal Waste' AND descriptor_2 = 'Dog'))",
    group_by = "community_board"
  )
)

mn_comparison_20_26 <- mn_dp_SRs_20_26 |> 
  left_join(mn_SRs_20_26) |> 
  filter(community_board != "Unspecified MANHATTAN")

fit <- lm(dp_count ~ count, data = mn_comparison_20_26)

ci <- predict(fit, interval = "confidence", level = 0.95)
mn_comparison_20_26$lwr <- ci[, "lwr"]
mn_comparison_20_26$upr <- ci[, "upr"]
mn_comparison_20_26$outlier <- with(mn_comparison_20_26,
                                     dp_count < lwr | dp_count > upr)

ggplot(mn_comparison_20_26,
       aes(x = count, y = dp_count)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  geom_text_repel(
    data = subset(mn_comparison_20_26, outlier),
    aes(label = community_board),   # swap in whatever your CB label column is
    size = 3,
    min.segment.length = 0,
    box.padding = 0.4
  )

mn_SRs_20_26_countOnly <- soc_read(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.json",
  query = soc_query(
    select = "count(*) as count",
    where = "borough = 'MANHATTAN'"
  )
)

comDist12_centerlines |> group_by(b5sc) |> summarize(the_geom = st_union(the_geom)) |> st_write("union_test.geojson")
# too big

manhattan_comDist <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Community_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson") |> 
  st_make_valid() |> 
  filter(BoroCD < 200) |> 
  mutate(community_board = paste0(
    str_sub(as.character(BoroCD), -2),
    " MANHATTAN"
  ))

mn_dp_SRs_24_26 <- soc_read(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.json",
  query = soc_query(
    where = "borough = 'MANHATTAN'
             AND created_date >= '2024-01-01T00:00:00'
             AND ((descriptor = 'Dog Waste') OR (descriptor = 'Animal Waste' AND descriptor_2 = 'Dog'))
    "
  )
)

mn_wkt <- st_as_text(st_geometry(st_union(manhattan_comDist))[[1]])

comDist12_dog_poop_SRs <- comDist12_dog_poop_SRs |> 
  st_join(
    comDist12_centerlines |> select(objectid),
    join = st_nearest_feature
  )


top_blocks <- comDist12_dog_poop_SRs |> 
  group_by(objectid) |> 
  summarize(n_dog_poop_SRs = n()) |> 
  arrange(desc(n_dog_poop_SRs)) |> 
  head(10)


top_blocks_manhattan_counts <- top_blocks_manhattan |> 
  st_drop_geometry() |> 
  filter(community_board != "12 MANHATTAN") |> 
  pull(n_dog_poop_SRs)

q[seq(3,34,3)]

max(top_blocks_manhattan_counts[seq(1,31,3)])
