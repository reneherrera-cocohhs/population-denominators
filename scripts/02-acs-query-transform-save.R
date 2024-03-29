# Setup ####
## package libraries ####
library(here)
library(tidyverse)
library(janitor)
library(tidycensus)

# Coconino Places ####
coco_zips <- read_csv(file = "../../OneDrive - Coconino County/Documents/coconino-county-zip-codes-and-communities.csv") %>%
  clean_names()

# make usable for searching 
coco_zip_names <- coco_zips$community %>%
  paste(collapse = "|")


coco_zip_names_north <- coco_zips %>%
  filter(region == "Northern")

coco_zip_names_north <- coco_zip_names_north$community %>%
  paste(collapse = "|")

# query and search variables 
v22 <- load_variables(2022, "acs5", cache = TRUE)

# view the variables 
View(v22)

# query ACS5 for variables of interest
az <- get_acs(
  geography = "place",
  variables = "B01001_001",
  cache_table = TRUE,
  year = 2022,
  state = "AZ",
  # county = "Coconino",
  survey = "acs5"
) %>%
  clean_names()

# filter to area

coco_places <- az %>%
  filter(str_detect(name, coco_zip_names)) 

coco_places

coco_places_north <- az %>%
  filter(str_detect(name, coco_zip_names_north)) 
