# Install packages
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

# Compile raw data (run only once, if needed)
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

# STEP 2: RUN MODEL AND EXPORT MODEL SUMMARY
source("models.R")

modelsummary(
  list(
    "FD"              = fd_model,
    "TWFE"            = twfe_main,
    "TWFE + NLW"      = twfe_nlw,
    "TWFE + NLW × Ed" = twfe_nlw_interaction
  ),
  stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map    = c("nobs", "r.squared", "adj.r.squared"),
  coef_omit  = "year",               # suppress year FE rows
  output     = "table.docx"          # or "table.tex", "table.html", "table.pdf"
)

