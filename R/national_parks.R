# 01. LOADING LIBRARIES ----
library(dplyr)
library(ggplot2)
library(plotly)


# 02. LOADING DATA ----
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")


# 03. VISUALIZATION ----
national_parks <- park_visits %>% 
    filter(unit_type == "National Park" & year != "Total") %>% 
    mutate(year = as.numeric(year)) %>% 
    select(year, visitors) %>% 
    group_by(year) %>% 
    mutate(total = sum(visitors)) %>% 
    select(-visitors) %>% 
    unique()

g1 <- ggplot(national_parks, aes(x = year, y = total / 1000000)) +
    geom_line(linetype = "dashed") +
    geom_area(fill = "Dark Green", alpha = 0.75) +
    labs(
        title = "U.S. National Parks have never been so popular",
        subtitle = "Annual recreational visits to national parks since 1904",
        x = "",
        y = "Visits (MM)",
        caption = "Source: data.world | Plot: @joaopcnogueira"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(
        limits = c(0,90),
        labels = c(0, 20, 40, 60, "80 MM"),
        breaks = c(0, 20, 40, 60, 80)
    )

ggplotly(g1)
