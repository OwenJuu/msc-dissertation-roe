# install.packages("devtools")
devtools::install_github("wklimowicz/tidyusoc")

rm(list = ls())

library(tidyusoc)
library(dplyr)
library(stringr)

## RUNNING THE LIBRARY TO CREATE THE RAW DATASET
usoc_convert(
  usoc_directory = "C:/Users/tp01040/Downloads/msc-dissertation-roe-r/UKDA-6614-spss/spss/spss28",
  new_directory = "C:/Users/tp01040/Downloads/msc-dissertation-roe-r/rds",
  filter_files = "indresp"
)

usoc <- usoc_compile(
  directory = "C:/Users/tp01040/Downloads/msc-dissertation-roe-r/rds"
)

## CLEANING THE DATASET
usoc_clean <- usoc %>%
  
  # Filtering out observations with no age
  filter(!is.na(age)) %>% 
  arrange(pidp, age) %>%
  group_by(pidp) %>%
  
  # Filtering out individual that joined USoc after 18 because we wouldn't
  # be able to know their full working history, unable to calculate years of experience
  filter(first(age) <= 18) %>%
  
  # Filling in the race varaible
  mutate(
    race = first(race[!race %in% c(NA, "inapplicable")])
  ) %>%
  
  ungroup()

usoc_clean <- usoc %>%
  # Clean qualification variable
  mutate(
    hiqual_dv = str_to_lower(hiqual_dv),
    hiqual_dv = str_trim(hiqual_dv),
    
    education_cat = case_when(
      hiqual_dv %in% c("no qualification", "no qual") ~ 
        "0_not_finished_high_school",
      
      hiqual_dv %in% c("gcse etc") ~ 
        "1_finished_middle_school",
      
      hiqual_dv %in% c("a level etc", "a-level etc") ~ 
        "2_finished_high_school",
      
      hiqual_dv == "degree" ~ 
        "3_undergraduate_degree",
      
      hiqual_dv %in% c("other qualification", "other qual") ~ 
        "4_non_undergrad_qualification",
      
      hiqual_dv %in% c("other higher", "other higher degree") ~ 
        "5_higher_degree",
      
      hiqual_dv %in% c("missing", "inapplicable", "refused", 
                       "refusal", "don't know") ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Clean job status variable
  mutate(
    jbstat = str_to_lower(jbstat),
    jbstat = str_trim(jbstat),
    
    isWorking = case_when(
      jbstat %in% c(
        "employed", 
        "self-employed", 
        "self employed", 
        "on furlough",
        "unpaid, family business", 
        "paid employment(ft/pt)", 
        "temporarily laid off/short term working"
      ) ~ 1,
      
      TRUE ~ 0
    )
  )


# DERIVE THE FINAL DATASET FOR REGRESSION
usoc_final <- usoc_clean %>%
  select(pidp, year, age, fimnlabgrs_dv, education_cat, jbstat, isWorking)

table(usoc_clean$jbstat)

model1 <- lm(fimnlabgrs_dv ~ age + education_cat, data = usoc_final)
summary(model1)