# female

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

# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_age_sex_usa <- get_acs(
  geography = "us",
  variables = c("Female" = "S0101_C05_001",
                "total" = "S0101_C01_001"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

female_usa <- acs5_age_sex_usa %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  mutate(estimate = Female/total) %>%
  select(NAME, estimate)

# sex female az ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_age_sex_az <- get_acs(
  geography = "state",
  state = "AZ",
  variables = c("Female" = "S0101_C05_001",
                "total" = "S0101_C01_001"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

female_az <- acs5_age_sex_az %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  mutate(estimate = Female/total) %>%
  select(NAME, estimate)

# sex female catchment ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_age_sex_catch <- get_acs(
  geography = "county",
  state = "AZ",
  variables = c("Female" = "S0101_C05_001",
                "total" = "S0101_C01_001"),
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

# for catchment
female_catchment <- acs5_age_sex_catch %>%
  subset_catchment() %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  summarise(Female = (sum(Female)),
            total = sum(total)) %>%
  mutate(estimate = Female/total,
         NAME = "Catchment") %>%
  select(NAME, estimate)

# for each county
female_catchment_counties <- acs5_age_sex_catch %>%
  subset_catchment() %>%
  select(NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  mutate(estimate = Female/total) %>%
  select(NAME, estimate)

# population by sex female for catchment each race ----
acs5_sex_catch_race <- get_acs(geography = "county",
                               state = "az",
                               variables = c(
                                 "white" = "B01001H_017",
                                 "hispanic" = "B01001I_017",
                                 "amer_indian" = "B01001C_017",
                                 "white_total" = "B01001H_001",
                                 "hisp_total" = "B01001I_001",
                                 "ai_total" = "B01001C_001"
                               ),
                               cache_table = TRUE,
                               year = 2019,
                               survey = "acs5"
)

female_catchment_race <- acs5_sex_catch_race %>%
  subset_catchment() %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  summarise(white = sum(white),
            hispanic = sum(hispanic),
            amer_indian = sum(amer_indian),
            white_total = sum(white_total),
            hisp_total = sum(hisp_total),
            ai_total = sum(ai_total)) %>%
  mutate(prop_white = white / white_total,
         prop_hisp = hispanic / hisp_total,
         prop_ai = amer_indian / ai_total) %>%
  select("Non-Hispanic White" = prop_white,
         "Hispanic" = prop_hisp,
         "American Indian" = prop_ai)

female_catchment_race <- female_catchment_race %>%
  pivot_longer(
    everything()
  ) %>%
  select("NAME" = "name",
         "estimate" = "value")

# population by sex for each county ----
# AGE AND SEX
# Survey/Program: American Community Survey
# Year: 2019
# Estimates: 5-Year
# Table ID: S0101
#
# Source: U.S. Census Bureau, 2019 American Community Survey 5-Year Estimates

acs5_age_sex <- get_acs(
  geography = "county",
  variables = c(
    "Female" = "S0101_C05_001",
    "Total" = "S0101_C01_001"
  ),
  state = "AZ",
  year = 2019,
  cache_table = TRUE,
  survey = "acs5"
)

female_catchment_counties <- acs5_age_sex %>%
  subset_catchment() %>%
  select(!(moe)) %>%
  spread(key = variable,
         value = estimate) %>%
  mutate(estimate = Female / Total)

# combine 
female_table <- bind_rows(
  female_usa,
  female_az,
  female_catchment,
  female_catchment_race,
  female_catchment_counties
)

female_table

# save to disk 
write_rds(female_table, "data/tidy/female.rds")
write_csv(female_table, "data/tidy/female.csv")
