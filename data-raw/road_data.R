## code to prepare `road_data` dataset goes here

library(openair)
library(worldmet)
library(dplyr)

# import AQ data
road_data <- importAURN(
  site = "my1",
  year = 1998:2016,
  hc = TRUE
)

# import met data
met <- importNOAA(year = 1998:2016)

# join together but ignore met data in road_data because it is modelled
road_data <-
  left_join(select(road_data, -ws, -wd, -air_temp), met, by = "date")

road_data <- select(
  road_data,
  date,
  nox,
  no2,
  ethane,
  isoprene,
  benzene,
  ws,
  wd,
  air_temp,
  RH,
  cl
)

road_data <- tibble(road_data)

usethis::use_data(road_data, overwrite = TRUE)
