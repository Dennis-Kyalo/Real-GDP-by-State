# Loading the libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(tidyquant)
library(fs)

# 1.0 Load the data and source the function
stategdp_wrangled_tbl <- read_rds(file = "00_data/stategdp_wrangled.rds")
source(file = "00_functions/all_functions.R") 

# Getting the metrics ----
metrics_data <- stategdp_wrangled_tbl %>% 
   time_picker_state(time_unit = "quarter") 

write_rds(metrics_data, file = "00_data/metrics_data.rds")
metrics_data <- read_rds(file = "00_data/metrics_data.rds") 

## First table ----

### Metric 1 - state GDP ----
metrics_data %>% 
    filter(year == "2005" & quarter == "Q1") %>% 
    filter(state %in% "United States") %>% 
    pull(gdp) %>% 
    scales::number(big.mark = ",", suffix = "M", prefix = "$")

### Metric 2 - state contribution to GDP ----
metrics_data %>% View() 
    filter(year == "2005" & quarter == "Q1") %>% 
    filter(state %in% "United States") %>% 
    pull(state_cont_percent) 

### Metric 3 - Contribution rank ----
metrics_data %>% 
    filter(year == "2005" & quarter == "Q1") %>% 
    filter(state %in% "Alabama") %>% 
    pull(rank)

### Metric 4 - Growth Rate ----
metrics_data %>%     
    select(state, gdp, format_date, quarter) %>%
    group_by(state) %>%
    mutate(lag_1 = lag(gdp, n = 1)) %>%
    mutate(lag_1 = case_when(is.na(lag_1) ~ gdp,
                             TRUE ~ lag_1)) %>%
    summarise(
        rate = (gdp - lag_1) / lag_1,
        row                   = row_number(),
        format_date           = format_date[row],
        quarter               = quarter[row],
        
    ) %>%
    ungroup() %>%
    mutate(rate_text = scales::percent(rate, accuracy = 0.1)) %>% 
    mutate(year = year(format_date)) %>% 
    filter(year == "2021" & quarter == "Q4") %>% 
    filter(state %in% "United States") %>% 
    pull(rate_text)


# fs::dir_create(path = "03_metrics")
# fs::dir_create(path = "04_css")
