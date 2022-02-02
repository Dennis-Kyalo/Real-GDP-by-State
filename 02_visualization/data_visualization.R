# Loading the libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(tidyquant)
library(plotly)
library(fs)

# 1.0 Load the data and source the function
stategdp_wrangled_tbl <- read_rds(file = "00_data/stategdp_wrangled.rds")
source(file = "00_functions/all_functions.R")


# 1.1 Time series Visualizations ----
# 1.1.0 GGplot based on one state GDP (Time-series) ----

g1 <- time_picker_state(stategdp_wrangled_tbl, time_unit = "quarter") %>%
    filter(state %in% "United States") %>%
    ggplot(aes(x = format_date, y = gdp)) +
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = state_cont_perc_text),
               color = "#2c3e50",
               size = 0.01) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_log10(labels = scales::dollar_format())

ggplotly(g1, tooltip = "text")


# 1.1.1 GGplot based on one state rate of change ----
g2 <- time_picker_state(stategdp_wrangled_tbl, time_unit = "quarter") %>%
    select(state, gdp, format_date) %>%
    group_by(state) %>%
    mutate(lag_1 = lag(gdp, n = 1)) %>% 
    mutate(lag_1 = case_when(
        is.na(lag_1) ~ gdp,
        TRUE ~ lag_1
    )) %>% 
    summarise(
        rate = (gdp - lag_1) / lag_1,
        row                   = row_number(),
        format_date           = format_date[row],
        
    ) %>% 
    ungroup() %>% 
    mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>% 
    mutate(state_cont_perc_text = str_glue("State : {state}
                                           Growth Rate : {rate_text}")) %>% 
    filter(state %in% c("United States")) %>% 
    
    ggplot(aes(x = format_date, y = rate)) +
    geom_line() +
    geom_point(aes(text = state_cont_perc_text),
               color = "#2c3e50",
               size = 0.01) +
    geom_smooth(method = "loess", span = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_tq() +
    expand_limits(y = 0) 


ggplotly(g2, tooltip = "text")


# US states maps function ----

pick_state <- function(data, state_name = "United States") {
    if (state_name == "United States") {
        
        option1 <- data %>%
            plot_geo(locationmode = "USA-states") %>%
            add_trace(
                z         = ~ gdp,
                locations = ~ abb,
                color     = ~ gdp,
                text      = ~ state_cont_perc_text,
                colors    = "Greens"
            ) %>%
            layout(geo = list(
                scope = "usa",
                projection = list(type = "albers usa"),
                showlakes  = TRUE,
                lakecolor  = toRGB("white")
            ))
        
        return(option1)        
        
        
        
    } else if (state_name != "United States") {
        
        
        option2 <- data %>%
            filter(state %in% state_name) %>%
            plot_geo(locationmode = "USA-states") %>%
            add_trace( z         = ~ gdp,
                       locations = ~ abb,
                       color     = ~ gdp,
                       text      = ~ state_cont_perc_text,
                       colors    = "Greens"
            ) %>%
            layout(geo = list(
                scope = "usa",
                projection = list(type = "albers usa"),
                showlakes  = TRUE,
                lakecolor  = toRGB("white")
                
            ))
        
        return(option2)    
    }
    
}


dump(list = c("time_picker_state", "pick_state"), file = "00_functions/all_functions.R")


# fs::dir_create(path = "02_visualization")










g2 <- time_picker_state(stategdp_wrangled_tbl, time_unit = "quarter") %>%
    filter(state %in% c("United States")) %>% 
select(state, gdp, format_date) %>%
    group_by(state) %>%
    mutate(lag_1 = lag(gdp, n = 1)) %>%
    mutate(lag_1 = case_when(is.na(lag_1) ~ gdp,
                             TRUE ~ lag_1)) %>%
    summarise(
        rate = (gdp - lag_1) / lag_1,
        row                   = row_number(),
        format_date           = format_date[row],
        
    ) %>%
    ungroup() %>%
    mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>%
    mutate(ind_cont_perc_text = str_glue("state : {state}
                                          Growth Rate : {rate_text}")) %>%
    mutate(pos = rate >= 0) %>%
    
    ggplot(aes(x = format_date, y = rate, fill = pos)) +
    geom_col() +
    # geom_line() +
    geom_point(aes(text = ind_cont_perc_text),
               color = "#2c3e50",
               size = 0.01) +
    # geom_smooth(method = "loess", span = 0.2) +
    theme_tq() +
    expand_limits(y = 0)

## Trial ----




time_picker_state(stategdp_wrangled_tbl, time_unit = "quarter") %>% 
    pick_state(state_name = "Texas")





library(viridis)
?viridis_pal


time_picker_state(stategdp_wrangled_tbl, time_unit = "quarter") %>%
    filter(state %in% "Texas") %>%
    plot_geo(locationmode = "USA-states") %>%
    add_trace( z         = ~ gdp,
               locations = ~ abb,
               color     = ~ gdp,
               text      = ~ state_cont_perc_text,
               colors    = "Viridis"
    ) %>%
    layout(geo = list(
        scope = "usa",
        projection = list(type = "albers usa"),
        showlakes  = TRUE,
        lakecolor  = toRGB("white")
        
    ))

