# Preample
install.packages("devtools", type = "win.binary")
devtools::install_github("wklimowicz/tidyusoc")

#Install the following packages if missing:
library(patchwork)
library(tidyverse)
library(tidyusoc)
library(dplyr)
library(stringr)
library(plm)
library(stargazer)
library(ivreg)       # FD-IV with diagnostics
library(fixest)      # TWFE / Sun & Abraham
library(modelsummary)
library(tsibble)
library(splines)
setwd("C:/Users/thuan/Downloads/msc-dissertation-roe/Data")

# Compile raw data (run only once)
usoc_convert(
  usoc_directory = "UKDA-6614-spss/spss/spss28",
  new_directory = "rds",
  filter_files = "indresp",
)

# Compile merged data
rm(list = ls())

custom_mappings <- function(cols) {
  life_sat <- pick_var(c("sclfsato", "lfsato"), cols)
  custom_variables <- tibble::tribble(
    ~usoc_name, ~new_name, ~type,
    "nchild_dv", "numChild", "numeric",
    "aidhh", "aidhh", "factor",
    "sex", "sex", "factor",
    "qfhigh_dv", "qfhigh_dv", "factor",
    "birthy", "birth_year", "numeric"
  )
  return(custom_variables)
}

usoc <- usoc_compile(
  directory = "rds",
  extra_mappings = custom_mappings
)

# Clean data (run only when adding new variables to save compile time)
source("Code/data_cleaning.R")

#Can be run freely 
rm(list = setdiff(ls(), c("usoc","usoc_df"))) #Removing everything except for the OG "usoc" dataset
gc()
source("Code/sample_selection.R")
source("Code/simple_model.R")
source("Code/plots.R")

