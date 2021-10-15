# Non-Hispanic White (Population)

# setup 
library(here)
library(tidyverse)
library(tidycensus)

var_acs_2019_acs5 <- load_variables(2019, "acs5", cache = TRUE)
var_acs_2019_acs5_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
var_acs_2019_acs5_subject <- load_variables(2019, "acs5/subject", cache = TRUE)
var_acs_2019_acs1 <- load_variables(2019, "acs1", cache = TRUE)

# function to filter to catchment counties only 
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

# population USA hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2019 American Community Survey 5-Year Estimates

acs5_hispanic_usa <- get_acs(
  geography = "us",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071P",
                "White alone Not Hispanic or Latino" = "DP05_0077P",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078P",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079P"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_hispanic_usa

# population AZ hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2019 American Community Survey 5-Year Estimates

acs5_hispanic_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071P",
                "White alone Not Hispanic or Latino" = "DP05_0077P",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078P",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079P"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_hispanic_az

# population catchment hispanic or latino ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2019 American Community Survey 5-Year Estimates

acs5_hispanic_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071",
                "White alone Not Hispanic or Latino" = "DP05_0077",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079",
                "Total" = "DP05_0070"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

val_catch_hisp <- acs5_hispanic_catch %>%
  subset_catchment() %>%
  filter(variable == "Total") %>%
  summarise(estimate = sum(estimate)) %>%
  as.numeric()

acs5_hispanic_catch <- acs5_hispanic_catch %>%
  subset_catchment() %>%
  filter(variable != "Total") %>%
  group_by(variable) %>%
  summarise(estimate = sum(estimate)) %>%
  mutate(total = val_catch_hisp,
         estimate = 100*(estimate / total)) %>%
  select(variable, estimate) %>%
  mutate(NAME = "Catchment")

# population race ethnicity for each county ----
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: DP05
#
# Source: U.S. Census Bureau, 2014-2019 American Community Survey 5-Year Estimates

acs5_catch_county_race <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Hispanic or Latino (of any race)" = "DP05_0071P",
                "White alone Not Hispanic or Latino" = "DP05_0077P",
                "Black or African American alone Not Hispanic or Latino" = "DP05_0078P",
                "American Indian and Alaska Native alone Not Hispanic or Latino" = "DP05_0079P" #,
                # "Total" = "DP05_0070"
                ),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_hispanic_catchment_counties <- acs5_catch_county_race %>%
  subset_catchment()

# population USA race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_usa <- get_acs(
  geography = "us",
  table = "B02001",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_usa %>%
  filter(variable == "B02001_002")

# population USA race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_usa %>%
  filter(variable == "B02001_004")

# population USA race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_usa %>%
  filter(variable == "B02001_003")

# population AZ race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az <- get_acs(
  geography = "state",
  state = "AZ",
  table = "B02001",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_az %>%
  filter(variable == "B02001_002")

# population AZ race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az %>%
  filter(variable == "B02001_004")

# population AZ race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az %>%
  filter(variable == "B02001_003")

# population catchment race White alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az_county <- get_acs(
  geography = "county",
  state = "AZ",
  table = "B02001",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_002") %>%
  summarise(total = sum(estimate))

# population catchment race American Indian and Alaska Native alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_004") %>%
  summarise(total = sum(estimate))

# population catchment race Black or African American alone ----
# RACE
# Survey/Program: American Community Survey
# Universe: Total population
# Year: 2019
# Estimates: 5-Year
# Table ID: B02001
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_population_race_az_county %>%
  mutate(NAME = str_replace(acs5_population_race_az_county$NAME, " County, Arizona", "")) %>%
  filter(NAME %in% counties) %>%
  filter(variable == "B02001_003") %>%
  summarise(total = sum(estimate))

# combine all together
race_ethnicity_table <- bind_rows(
  acs5_hispanic_usa,
  acs5_hispanic_az,
  acs5_hispanic_catch,
  acs5_hispanic_catchment_counties
)

# save to disk 
write_rds(race_ethnicity_table, "data/tidy/race_ethnicity_table.rds")
write_csv(race_ethnicity_table, "data/tidy/race_ethnicity_table.csv")
