# population 

# setup 
library(here)
library(tidyverse)
library(tidycensus)

var_acs_2019_acs5 <- load_variables(2019, "acs5", cache = TRUE)
var_acs_2019_acs5_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
var_acs_2019_acs5_subject <- load_variables(2019, "acs5/subject", cache = TRUE)
var_acs_2019_acs1 <- load_variables(2019, "acs1", cache = TRUE)

# total population usa ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_total_usa <- get_acs(
  geography = "us",
  table = "B01003",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_usa

# total population az ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_total_az <- get_acs(
  geography = "state",
  state = "AZ",
  table = "B01003",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_total_az

# total population catchment ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_total_az_county <- get_acs(
  geography = "county",
  table = "B01003",
  state = "AZ",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# catchment counties only 
# function 
subset_catchment <- function(x){
  counties <- c(
    "Cochise County, Arizona",
    "Pima County, Arizona",
    "Pinal County, Arizona",
    "Santa Cruz County, Arizona",
    "Yuma County, Arizona"
  )
  x %>%
    filter(x$NAME %in% counties)
}

acs5_population_total_uazcc <- acs5_population_total_az_county %>%
  subset_catchment() %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(NAME = "Catchment")

acs5_population_total_uazcc

# total population County ----
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B01003
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_total_az_county %>%
  subset_catchment()

population <- 
bind_rows(
  acs5_population_total_usa,
  acs5_population_total_az,
  acs5_population_total_uazcc,
  acs5_population_total_az_county
)

# save to disk 
write_rds(population, "data/tidy/population.rds")
write_csv(population, "data/tidy/population.csv")
