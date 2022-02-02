# Loading the libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(tidyquant)
library(fs)

# Creating data directory
fs::dir_create(path = "00_data")

# 1.1 Wrangling the data ----
# Load the data
state_gdp_tbl <- read_xlsx(path = "00_data/GDP_by_State.xlsx")
state_coordinates <- read_csv(file = "00_data/statelatlong.csv")
state_gdp_tbl %>% View()

stategdp_wrangled_tbl <-  state_gdp_tbl %>%
    pivot_longer(
        cols      = c(-GeoName, -GeoFips),
        names_to  = "date",
        values_to = "gdp"
    ) %>%
    # Separating the year column to year and quarter
    separate(col  = "date",
             into = c("year", "quarter"),
             sep  = ":") %>%
    
    # converting the quarters into quarterly dates
    mutate(
        quarterly_date = case_when(
            quarter    == "Q1" ~ "01-01",
            quarter    == "Q2" ~ "04-01",
            quarter    == "Q3" ~ "07-01",
            quarter    == "Q4" ~ "10-01"
        )
    ) %>%
    
    # combining the quarterly dates with year and renaming year column
    mutate(year = year %>% str_c(... = quarterly_date, sep = "-")) %>%
    mutate(year = ymd(year)) %>%
    rename(gdp_date = year) %>%
    
    # removing quarterly date column
    select(-quarterly_date) %>% 
    mutate(GeoName = tolower(GeoName)) %>%  
    left_join(state.regions, by = c("GeoName" = "region")) %>% 
    select(-fips.numeric, -fips.character) %>% 
    mutate(GeoName = str_to_title(GeoName)) %>% 
    filter(
        !GeoName %in% c("New England", 
                       "Mideast", 
                       "Great Lakes", 
                       "Plains", 
                       "Southeast", 
                       "Southwest",
                       "Rocky Mountain",
                       "Far West")
    ) %>% 
    left_join(state_coordinates, by = c("abb" = "State")) %>% 
    select(-City)
    
write_rds(stategdp_wrangled_tbl, file = "00_data/stategdp_wrangled.rds")

data("df_pop_state")
df_pop_state %>% View()
data(state.regions)
head(state.regions)




