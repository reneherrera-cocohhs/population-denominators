# Setup ####

# List of packages needed ####
packages_needed_list <- c(
  "here", # https://github.com/r-lib/here
  "tidyverse", # https://github.com/tidyverse/tidyverse
  "pins", # https://github.com/rstudio/pins
  "lubridate", # https://github.com/tidyverse/lubridate
  "janitor", # https://github.com/sfirke/janitor
  "readxl", # https://github.com/tidyverse/readxl
  "curl", # https://github.com/jeroen/curl
  "purrr", # https://github.com/tidyverse/purrr
  "tidycensus", # https://github.com/walkerke/tidycensus
  "zipcodeR", # https://github.com/gavinrozzi/zipcodeR/
  "tigris", # https://github.com/walkerke/tigris
  "renv",
  "styler"
)

# function #### source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])] # check to see if packages are installed
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  } # Install them if they are not
  # sapply(pkg, require, character.only = TRUE) # , then load them into the R session.
}

# call function ####
ipak(packages_needed_list)

# create new directories and files 
dir.create("scripts")
file.create("scripts/00-setup.R")
file.create("scripts/01-adhs-query-transform-save.R")
file.create("scripts/02-acs-query-transform-save.R")
file.create("README.md")
dir.create("reports")
dir.create("figures")
dir.create("data-raw")
dir.create("data-tidy")


