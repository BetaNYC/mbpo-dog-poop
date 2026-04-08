library(flextable)
library(sf)
library(tidyverse)
library(socratadata)
library(lubridate)


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
