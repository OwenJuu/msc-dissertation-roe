# Install packages
install.packages("devtools")
devtools::install_github("wklimowicz/tidyusoc")
library(tidyusoc)
library(dplyr)
library(stringr)
library(AER)
library(plm)
library(stargazer)

setwd("C:/Users/tp01040/Downloads/msc-dissertation-roe-r")

# Create raw data (run only once, if needed)
usoc_convert(
  usoc_directory = "UKDA-6614-spss/spss/spss28",
  new_directory = "rds",
  filter_files = "indresp",
)

# STEP 1: CLEAN DATA
rm(list = ls())

usoc <- usoc_compile(
  directory = "rds"
)

source("data_cleaning.R")

# STEP 2: RUN MODEL
source("models.R")

