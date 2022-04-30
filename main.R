library(dplyr)
library(readr)
library(ggplot2)

transport_file <- "data/region_traffic_by_vehicle_type.csv"
population_file <- "data/population_clean.csv"

transport_tbl <- read_csv(file = transport_file)
population_tbl <- read_csv(file = population_file)
