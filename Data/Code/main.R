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
library(did)         # Callaway & Sant'Anna
library(modelsummary)
library(tsibble)
library(pandoc)
library(sampleSelection)
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
    "sex", "sex", "factor"
  )
  return(custom_variables)
}

usoc <- usoc_compile(
  directory = "rds",
  extra_mappings = custom_mappings
)

# Clean data
source("Code/data_cleaning.R")

# Run model and export model summary
source("Code/models.R")
summary(probit_sel)
summary(twfe_iv, stage = 1:2)
summary(twfe_iv2, stage = 1:2)

source("Code/plots.R")


