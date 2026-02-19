# install.packages("devtools")
devtools::install_github("wklimowicz/tidyusoc")

library(tidyusoc)
library(dplyr)

# Convert to Rds to save space and time
usoc_convert(usoc_directory = "C:/Users/tp01040/Downloads/msc-dissertation-roe-r/UKDA-6614-spss/spss/spss28",
             new_directory = "rds",
             filter_files = "indresp")

# Compile the INDRESP files in the rds folder
usoc <- usoc_compile(directory = "rds")

usoc_clean <- usoc %>%
  select(-hidp, -wave, -waveid, -country) %>%
  select(pidp, year, age, everything()) %>%
  group_by(pidp)

usoc_balanced_complete <- usoc_clean %>%
  group_by(pidp) %>%
  filter(n_distinct(year) == 18) %>%
  ungroup()
