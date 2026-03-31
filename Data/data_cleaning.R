# data_cleaning.R
NLW <- read.csv("NLW.csv")

# CLEAN DATA
usoc_clean <- usoc %>%
  filter(!is.na(age)) %>% 
  arrange(pidp, age) %>%
  group_by(pidp) %>%
  filter(first(age) <= 18) %>% #We must starts from 18 years old, otherwise we can't count expyrs
  mutate(
    pidp = as.integer(pidp),
    year = as.integer(as.character(year)),
    race = first(race[!race %in% c(NA, "inapplicable")]), #Filling in all the missing race value
    
    # Create highest qualification variable
    hiqual_dv = str_to_lower(hiqual_dv),
    hiqual_dv = str_trim(hiqual_dv),
    hiqual_cat = case_when(
      hiqual_dv %in% c("no qualification", "no qual") ~ "0_not_finished_middle_school",
      hiqual_dv %in% c("gcse etc") ~ "1_finished_middle_school",
      hiqual_dv %in% c("a level etc", "a-level etc") ~ "2_finished_high_school",
      hiqual_dv == "degree" ~ "3_undergraduate_degree",
      hiqual_dv %in% c("other qualification", "other qual") ~ "4_non_undergrad_qualification",
      hiqual_dv %in% c("other higher", "other higher degree") ~ "5_higher_degree",
      TRUE ~ NA_character_
    ),
    
    # Create binary educ variable
    middleSchool = ifelse(hiqual_cat %in% c("0_not_finished_middle_school", NA), 0, 1),
    highSchool = ifelse(hiqual_cat %in% c("0_not_finished_middle_school", "1_finished_middle_school", NA), 0, 1),
    undergrad = ifelse(hiqual_cat %in% c("0_not_finished_middle_school", "1_finished_middle_school", "2_finished_high_school", NA), 0, 1),
    higherEd = ifelse(hiqual_cat %in% c("0_not_finished_middle_school", "1_finished_middle_school", "2_finished_high_school", "3_undergraduate_degree", "4_non_undergrad_qualification", NA), 0, 1),
    
    #Handling expryrs
    jbstat = str_to_lower(jbstat),
    jbstat = str_trim(jbstat),
    isWorking = case_when(
      jbstat %in% c("employed","self-employed","self employed","on furlough",
                    "unpaid, family business","paid employment(ft/pt)",
                    "temporarily laid off/short term working") ~ 1,
      TRUE ~ 0
    ),
    expyrs = cumsum(isWorking),
    
    #Calculating log of wage
    lwage = log(fimnlabgrs_dv + 1), #TODO: change this later
  )

#Import NLW variable into the dataset
usoc_clean <- left_join(usoc_clean, NLW, by = "year")

# FINAL DATASET
usoc_df <- usoc_clean %>%
  select(pidp, year, lwage, middleSchool, highSchool, undergrad, higherEd, expyrs, 
         NLW)
usoc_tsibble <- usoc_df %>% as_tsibble(key = pidp, index = year)
usoc_plm<- pdata.frame(usoc_df, index = c("pidp", "year"))
