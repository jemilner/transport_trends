library(dplyr)
library(readr)
library(ggplot2)

transport_file <- "data/region_traffic_by_vehicle_type.csv"
population_file <- "data/population_clean.csv"

transport_tbl <- read_csv(file = transport_file)
population_tbl <- read_csv(file = population_file)

summary(transport_tbl)
summary(population_tbl)

#join region name and pop count from population_tbl to transport_tbl
combo_tbl <- transport_tbl %>%
    left_join(
        population_tbl,
        by = c("ons_code" = "Code", "year" = "Year")
    ) %>%
    arrange(year, region_id)


