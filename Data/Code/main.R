# Preample
install.packages("devtools")
devtools::install_github("wklimowicz/tidyusoc")
install.packages("tidyverse")
install.packages("tsibble")
install.packages("pandoc")
install.packages("sampleSelection")
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
setwd("C:/Users/tp01040/Downloads/msc-dissertation-roe/Data/")

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
summary(interaction, stage = 1:2)



##OUTPUT SUMMARY THINGIES
etable(
  twfe_iv,
  stage = 1,
  fitstat = ~ ivf1 + wald,
  tex = TRUE,
  file = "first_stage_table.tex"
)

etable(
  interaction,
  stage = 2,
  fitstat = ~ ivf1 + wald,
  tex = TRUE,
  file = "second_stage_table.tex"
)

#FIRST STAGE TABLE
fs <- summary(twfe_iv, stage = 1)

modelsummary(
  list(
    "ALevel" = fs[[1]],
    "Undergrad" = fs[[2]],
    "HigherEd" = fs[[3]],
    "Experience" = fs[[4]]
  ),
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "AIC|BIC|RMSE",
  notes = "First-stage regressions. Clustered standard errors at the individual level.",
  output = "first_stage_table.tex"
)

#SECOND STAGE
modelsummary(
  list(
    "TWFE IV" = twfe_iv,
    "TWFE IV w/ interaction" = twfe_iv2
  ),
  stars = TRUE,
  statistic = "({std.error})",
  gof_omit = "AIC|BIC|RMSE",
  notes = "Clustered standard errors at the individual level.",
  output = "second_stage_table.tex"
)

table(usoc$race)


##TEST MODEL
source("test_model.R")
