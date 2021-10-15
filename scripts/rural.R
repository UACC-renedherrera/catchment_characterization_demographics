# Rural 

# setup 
library(here)
library(tidyverse)
library(tidycensus)

var_acs_2019_acs5 <- load_variables(2019, "acs5", cache = TRUE)
var_acs_2019_acs5_profile <- load_variables(2019, "acs5/profile", cache = TRUE)
var_acs_2019_acs5_subject <- load_variables(2019, "acs5/subject", cache = TRUE)
var_acs_2019_acs1 <- load_variables(2019, "acs1", cache = TRUE)

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


# population urban and rural usa ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2010 Census.

population_rural_usa_h2 <- 27684999 / 131704730
population_rural_usa_h2 <- as_tibble(population_rural_usa_h2) %>%
  mutate(NAME = "United States") %>%
  rename(estimate = value)

# from table P2:
population_rural_usa_p2 <- as_tibble(59492267 / 308745538) %>%
  mutate(NAME = "United States") %>%
  rename(estimate = value)
population_rural_usa_p2

# population urban and rural az ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

population_rural_az_h2 <- as_tibble(329022 / 2844526) %>%
  mutate(NAME = "Arizona") %>%
  rename(estimate = value)
population_rural_az_h2

# from table P2:
population_rural_az_p2 <- as_tibble(651358 / 6392017) %>%
  mutate(NAME = "Arizona") %>%
  rename(estimate = value)
population_rural_az_p2

# population urban and rural catchment ----
# URBAN AND RURAL
# Survey/Program: Decennial Census
# Universe: Housing units
# Year: 2010
# Table ID: H2
#
# Source: U.S. Census Bureau, 2018 American Community Survey 5-Year Estimates

# the following values were obtained from looking at the web page of the H2 table 
# population_rural_catch_h2 <- tibble(
#   NAME = c(
#     "AZ",
#     "Cochise",
#     "Pima",
#     "Pinal",
#     "Santa Cruz",
#     "Yuma"
#   ),
#   estimate = c(
#     329022,
#     22851,
#     33543,
#     30511,
#     6446,
#     NA
#   ),
#   total = c(
#     "AZ",
#     "Cochise",
#     "Pima",
#     "Pinal",
#     "Santa Cruz",
#     "Yuma"
#   )
# )

# section below seems to have broken 
# I commented it out

# population_rural_catch_h2 %>%
#   filter(NAME != "AZ") %>%
#   drop_na() %>%
#   summarise(estimate = sum(estimate)) %>%
#   mutate(NAME = "Catchment")
# 
# population_rural_catch_h2 <- population_rural_catch_h2 %>%
#   mutate(total = 329022,
#          estimate = estimate / total)

# from table P2:
# population_rural_catch_p2 <- tibble(
#   NAME = c(
#     "AZ",
#     "Cochise",
#     "Pima",
#     "Pinal",
#     "Santa Cruz",
#     "Yuma"
#   ),
#   estimate = c(
#     651358,
#     47680,
#     73750,
#     82311,
#     12748,
#     20416
#   ),
#   total = c(
#     6392017,
#     131346,
#     980263,
#     375770,
#     47420,
#     195751
#   )
# )
# 
# # for the catchment altogether
# population_rural_catch_p2 %>%
#   filter(NAME != "AZ") %>%
#   drop_na() %>%
#   summarise(estimate_total = sum(estimate),
#             total = sum(total)) %>%
#   mutate(estimate = estimate_total / total)
# 
# # for each catchment county
# population_rural_catch_h2 %>%
#   filter(NAME != "AZ") %>%
#   mutate(estimate = estimate / total) %>%
#   select(NAME, estimate)


# combine the rural percentage altogether to populate the characterization table
# bind_rows(
#   population_rural_usa_h2,
#   population_rural_az_h2,
#   population_rural_catch_h2
# ) %>%
#   select(NAME, estimate)
