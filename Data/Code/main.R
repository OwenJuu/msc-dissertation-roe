# Preample
install.packages("devtools", type = "win.binary")
devtools::install_github("wklimowicz/tidyusoc")
devtools::install_github("yuchang0321/IVQR")

#Install the following packages if missing:
library(IVQR)
library(readxl)
library(patchwork)
library(tidyverse)
library(tidyusoc)
library(dplyr)
library(stringr)
library(plm)
library(gtsummary)
library(fixest)      # TWFE / Sun & Abraham
library(modelsummary)
library(tsibble)
library(splines)
library(MASS)
setwd("C:/Users/tp01040/Downloads/msc-dissertation-roe/Data")

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
    "birthy", "birth_year", "numeric",
    "maedqf", "momeduc", "factor",
    "cgwrd_dv", "ability", "factor"
  )
  return(custom_variables)
}

usoc <- usoc_compile(
  directory = "rds",
  extra_mappings = custom_mappings
)

#Removing everything except for the OG "usoc" dataset
rm(list = setdiff(ls(), c("usoc"))) 
gc()

# Clean data (run only when adding new variables to save compile time)
source("Code/0_data_cleaning.R")

#Removing everything except for the OG "usoc" dataset, and freeing some memory
rm(list = setdiff(ls(), c("usoc","usoc_df", "mw_cpi", "regional_hep", "regional_unemp"))) 
gc()

source("Code/1_sample_selection.R")
source("Code/2_simple_model.R")

##Exporting model's result
etable(
  simple_linear, iv_panel
  #tex = TRUE,
  #file = "results.tex"
)
source("Code/plots.R")

