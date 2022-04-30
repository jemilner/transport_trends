library(dplyr)
library(readr)
library(ggplot2)

source("R/helper_fns.R")

transport_file <- "data/region_traffic_by_vehicle_type.csv"
population_file <- "data/population_clean.csv"

transport_tbl <- read_csv(file = transport_file)
population_tbl <- read_csv(file = population_file) %>%
    rename("region_name" = "Government Office Regions") %>%
    rename("population" = "All Persons")


summary(transport_tbl)
summary(population_tbl)

#join region name and pop count from population_tbl to transport_tbl
combo_tbl <- transport_tbl %>%
    left_join(
        population_tbl,
        by = c("ons_code" = "Code", "year" = "Year")
    ) %>%
    arrange(year, region_id)

#create baseline comparison
combo_tbl <- combo_tbl %>%
    mutate(pedal_cycles_change = round(pedal_cycles / get_baseline(combo_tbl, region_id, "pedal_cycles"), 2)) %>%
    mutate(cars_and_taxis_change = round(cars_and_taxis / get_baseline(combo_tbl, region_id, "cars_and_taxis"), 2)) %>%
    mutate(buses_and_coaches_change = round(buses_and_coaches / get_baseline(combo_tbl, region_id, "buses_and_coaches"), 2)) %>%
    mutate(lgvs_change = round(lgvs / get_baseline(combo_tbl, region_id, "lgvs"), 2)) %>%
    mutate(all_hgvs_change = round(all_hgvs / get_baseline(combo_tbl, region_id, "all_hgvs"), 2)) %>%
    mutate(all_motor_vehicles_change = round(all_motor_vehicles / get_baseline(combo_tbl, region_id, "all_motor_vehicles"), 2)) %>%
    mutate(population_change = round(population / get_baseline(combo_tbl, region_id, "population"), 2))