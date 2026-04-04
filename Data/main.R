# Preample
install.packages("devtools")
devtools::install_github("wklimowicz/tidyusoc")
install.packages("tidyverse")
install.packages("tsibble")
install.packages("pandoc")
library(tidyverse)
library(tidyusoc)
library(dplyr)
library(stringr)
library(plm)
library(stargazer)
library(ivreg)       # FD-IV with diagnostics
library(fixest)      # TWFE / Sun & Abraham
library(did)         # Callaway & Sant'Anna
library(modelsummary)
library(tsibble)
library(pandoc)
setwd("C:/Users/tp01040/Downloads/msc-dissertation-roe/Data")

# Compile raw data (run only once)
usoc_convert(
  usoc_directory = "UKDA-6614-spss/spss/spss28",
  new_directory = "rds",
  filter_files = "indresp",
)

# Clean data
rm(list = ls())
custom_mappings <- function(cols) {
  
  life_sat <- pick_var(c("sclfsato", "lfsato"), cols)
  
  custom_variables <- tibble::tribble(
    ~usoc_name, ~new_name, ~type,
    "nchild_dv", "nchild_dv", "numeric"
  )
  
  return(custom_variables)
  
}
usoc <- usoc_compile(
  directory = "rds",
  extra_mappings = custom_mappings
)
source("data_cleaning.R")

# Run model and export model summary
source("models.R")


