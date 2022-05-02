library(dplyr)
library(readr)
library(tidyr)
library(glue)
library(ggplot2)
library(ggtext)
library(viridis)

source("R/helper_fns.R")

transport_file <- "data/region_traffic_by_vehicle_type.csv"
population_file <- "data/population_clean.csv"

transport_tbl <- read_csv(file = transport_file) %>%
    rename("motorbikes" = "two_wheeled_motor_vehicles")
population_tbl <- read_csv(file = population_file) %>%
    rename("region_name" = "Government Office Regions") %>%
    rename("population" = "All Persons")

min_year <- 1995
max_year <- 2019

# summary(transport_tbl)
# summary(population_tbl)

#join region name and pop count from population_tbl to transport_tbl
regional_tbl <- transport_tbl %>%
    left_join(
        population_tbl,
        by = c("ons_code" = "Code", "year" = "Year")
    ) %>%
    arrange(year, region_id)

#create baseline comparison
regional_tbl <- regional_tbl %>%
    mutate(pedal_cycles_change = round(pedal_cycles / get_baseline_region(regional_tbl, region_id, "pedal_cycles"), 2)) %>%
    mutate(motorbikes_change = round(motorbikes / get_baseline_region(regional_tbl, region_id, "motorbikes"), 2)) %>%
    mutate(cars_and_taxis_change = round(cars_and_taxis / get_baseline_region(regional_tbl, region_id, "cars_and_taxis"), 2)) %>%
    mutate(buses_and_coaches_change = round(buses_and_coaches / get_baseline_region(regional_tbl, region_id, "buses_and_coaches"), 2)) %>%
    mutate(lgvs_change = round(lgvs / get_baseline_region(regional_tbl, region_id, "lgvs"), 2)) %>%
    mutate(all_hgvs_change = round(all_hgvs / get_baseline_region(regional_tbl, region_id, "all_hgvs"), 2)) %>%
    mutate(all_motor_vehicles_change = round(all_motor_vehicles / get_baseline_region(regional_tbl, region_id, "all_motor_vehicles"), 2)) %>%
    mutate(population_change = round(population / get_baseline_region(regional_tbl, region_id, "population"), 2))

#filter on just England and between min and max year
regional_tbl <- regional_tbl %>%
    slice(grep("^E", ons_code)) %>%
    filter(year >= min_year & year <= max_year)

#exploratory plot
# ggplot(regional_tbl, aes(x = year, y = population_change, group = region_name)) +
#     geom_line(aes(color = as.factor(region_name)))

#national tbl
national_tbl <- regional_tbl %>%
    group_by(year) %>%
    summarise(
        population = sum(population),
        pedal_cycles = sum(pedal_cycles),
        motorbikes = sum(motorbikes),
        cars_and_taxis = sum(cars_and_taxis),
        buses_and_coaches = sum(buses_and_coaches),
        lgvs = sum(lgvs),
        all_hgvs = sum(all_hgvs),
        all_motor_vehicles = sum(all_motor_vehicles)
    ) 

#create baseline comparison and just keep required cols in national_tbl
national_tbl <- national_tbl%>%
    mutate(population_change = round(population / get_baseline_national(national_tbl, "population"), 2)) %>%
    mutate(pedal_cycles_change = round(pedal_cycles / get_baseline_national(national_tbl, "pedal_cycles"), 2)) %>%
    mutate(motorbikes_change = round(motorbikes / get_baseline_national(national_tbl, "motorbikes"), 2)) %>%
    mutate(cars_and_taxis_change = round(cars_and_taxis / get_baseline_national(national_tbl, "cars_and_taxis"), 2)) %>%
    mutate(buses_and_coaches_change = round(buses_and_coaches / get_baseline_national(national_tbl, "buses_and_coaches"), 2)) %>%
    mutate(lgvs_change = round(lgvs / get_baseline_national(national_tbl, "lgvs"), 2)) %>%
    mutate(all_hgvs_change = round(all_hgvs / get_baseline_national(national_tbl, "all_hgvs"), 2)) %>%
    mutate(all_motor_vehicles_change = round(all_motor_vehicles / get_baseline_national(national_tbl, "all_motor_vehicles"), 2)) %>%
    select(-c(
        population,
        pedal_cycles,
        motorbikes,
        cars_and_taxis,
        buses_and_coaches,
        lgvs,
        all_hgvs,
        all_motor_vehicles
    ))

#from viridis::inferno(n = 5)
pop_colour <- "#56106EFF"
car_colour <- "#F98C0AFF"
bus_colour <- "#BB3754FF"

## plot national trends
ggplot(national_tbl %>%
           select(all_of(c(
               "year",
               "population_change",
               "cars_and_taxis_change",
               "buses_and_coaches_change"
           ))) %>%
           pivot_longer(cols = -year), 
       aes(year, value, group_by(name))) +
    geom_line(
        aes(color = name),
        size = 1.2
    ) +
    scale_color_manual(
        values = c(
            population_change = pop_colour,
            cars_and_taxis_change = car_colour,
            buses_and_coaches_change = bus_colour)
    ) +
    coord_cartesian(
        xlim = c(min_year, max_year),
        ylim = c(0.6, 1.4),
        expand = F,
        clip = "off"
    ) +
    scale_x_continuous(
        breaks = seq(min_year, max_year, 4),
        position = "top"
    ) +
    scale_y_continuous(
        breaks = seq(0.6, 1.4, 0.2),
        labels = paste("x ", seq(0.6, 1.4, 0.2), sep = "")
    ) +
    labs(
        title = "England's population and transport mileage 1995-2019",
        subtitle = "Compared to the 1995 baseline",
        x = "Year",
        y = "Relative to 1995 baseline",
        caption = paste0(
            "\nData source: Department for Transport (mileage) & ONS (population)\n",
            "Graphic: Jordan Milner (@JordMilner9)"
        )
    ) + 
    geom_hline(
        yintercept = 1,
        color = "grey40",
        linetype = "dashed"
    ) +
    theme_minimal() +
    theme(
        axis.line = element_line(colour = "grey40"),
        axis.ticks = element_line(colour = "grey40"),
        axis.text = element_text(colour = "grey40"),
        panel.grid = element_blank(),
        text = element_text(colour = "grey40"),
        plot.title = element_text(colour = "black"),
        plot.title.position = "plot",
        #top, right, bottom, left
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        plot.caption = element_text(size = 8, hjust = 0),
        plot.caption.position = "plot",
        legend.position = "none"
    ) +
    annotate(
        "richtext",
        x = 2007,
        y = 0.6,
        label = glue(
            paste0(
                "<span style = 'color:{pop_colour}'>Population</span> | ",
                "<span style = 'color:{car_colour}'>Cars and taxis</span> | ",
                "<span style = 'color:{bus_colour}'>Buses and coaches</span>"
            )
        ),
        hjust = 0.5,
        vjust = 0,
        size = 4,
        label.color = NA,
        fill = NA
    ) + 
    annotate(
        "richtext",
        x = 2006,
        y = 1.25,
        label = glue(
            paste0(
                "<span style = 'color:{car_colour}'>Car and taxi</span> mileage growth<br>",
                "has regularly outpaced<br>",
                "population growth"
            )
        ),
        hjust = 0,
        vjust = 0.5,
        size = 3,
        label.color = NA,
        fill = NA
    ) + 
    annotate(
        "richtext",
        x = 2008,
        y = 0.9,
        label = glue(
            paste0(
                "<span style = 'color:{bus_colour}'>Bus and coach</span> mileage<br>",
                "has been decreasing<br>",
                "for over 10 years"
            )
        ),
        hjust = 0,
        vjust = 1,
        size = 3,
        label.color = NA,
        fill = NA
    ) + 
    annotate(
        "text",
        x = 2019,
        y = 1.01,
        label = "1995 baseline",
        hjust = 1,
        vjust = 0,
        size = 3,
        colour = "grey40"
    )

