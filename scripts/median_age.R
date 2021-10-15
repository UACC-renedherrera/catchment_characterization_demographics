# median age

# setup 
library(here)
library(tidyverse)
library(tidycensus)

var_acs_2019_acs5 <- load_variables(2019, "acs5", cache = TRUE)
var_acs_2019_acs5_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
var_acs_2019_acs5_subject <- load_variables(2019, "acs5/subject", cache = TRUE)
var_acs_2019_acs1 <- load_variables(2019, "acs1", cache = TRUE)

# median age usa ----

acs5_median_age_usa <- get_acs(
  geography = "us",
  variables = c("median_age" = "B01002_001"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

acs5_median_age_usa

# median age az ----

acs5_median_age_az <- get_acs(
  geography = "state",
  state = "az",
  variables = c("median_age" = "B01002_001"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

acs5_median_age_az

# median age catchment ----
# all az counties 

acs5_median_age_az_county <- get_acs(
  geography = "county",
  variables = c(
    "median_age" = "B01002_001"
  ),
  cache_table = TRUE,
  year = 2019,
  state = "az",
  survey = "acs5"
)

acs5_median_age_az_county

# median age data table
table_median_age <- bind_rows(acs5_median_age_usa, 
                              acs5_median_age_az,
                              acs5_median_age_az_county) %>%
  #select(GEOID, NAME, variable, estimate) %>%
  mutate(variable = "median age",
         race = "all",
         sex = "all",
         year = "2014-2019",
         source = "US Census: ACS5")

table_median_age

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

acs5_median_age_az_county %>%
  subset_catchment()

# range of catchment counties only 
acs5_median_age_az_county %>%
  subset_catchment %>%
  summarise(min(estimate),
            max(estimate))

# Median age catchment race ----
# MEDIAN AGE BY SEX (HISPANIC OR LATINO)
# Survey/Program: American Community Survey
# Universe: People who are Hispanic or Latino
# Year: 2019
# Estimates: 5-Year
# Table ID: B01002I
#
# Source: U.S. Census Bureau, 2014-2019 American Community Survey 5-Year Estimates

median_age <- get_acs(
  geography = "county",
  state = "az",
  variables = c("median_age_white" = "B01002H_001",
                "median_age_hisp" = "B01002I_001",
                "median_age_ai" = "B01002C_001",
                "median_age_black" = "B01002B_001"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5"
)

glimpse(median_age)

# function to generate range of median age values for each race
get_range_of_median_age <- function(x, race){
  x %>%
    filter(variable == race) %>%
    summarise(min(estimate),
              max(estimate))
}

# median age for WHITE ALONE, NOT HISPANIC OR LATINO in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_white")

# median age for BLACK OR AFRICAN AMERICAN ALONE in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_black")

# median age for HISPANIC OR LATINO in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_hisp")

# median age for AMERICAN INDIAN AND ALASKA NATIVE ALONE in catchment
median_age %>%
  subset_catchment() %>%
  get_range_of_median_age("median_age_ai")

# assemble table for median age including races
median_age <- median_age %>%
  mutate(race = case_when(
    variable == "median_age_white" ~ "WHITE ALONE, NOT HISPANIC OR LATINO",
    variable == "median_age_ai" ~ "AMERICAN INDIAN AND ALASKA NATIVE ALONE",
    variable == "median_age_hisp" ~ "HISPANIC OR LATINO",
    variable == "median_age_black" ~ "BLACK OR AFRICAN AMERICAN ALONE",
    TRUE ~ "NA"
  ),
  sex = "all",
  year = "2014-2019",
  source = "US Census: ACS5",
  variable = "median age")

glimpse(median_age)

# assemble table for median age for all known values 
table_median_age <- bind_rows(table_median_age, median_age)
glimpse(table_median_age)
table_median_age

# save 
write_rds(table_median_age, "data/tidy/median_age.rds")
write_csv(table_median_age, "data/tidy/median_age.csv")
