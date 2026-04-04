# data_cleaning.R
macro <- read.csv("macro.csv")

# CLEAN DATA
usoc_clean <- usoc %>%
  filter(!is.na(year)) %>% 
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  filter(first(age) <= 18) %>% #We must starts from 18 years old, otherwise we can't count expyrs
  mutate(
    pidp = as.integer(pidp),
    year = as.integer(as.character(year)),
    race = first(race[!race %in% c(NA, "inapplicable")]), #Filling in all the missing race value
    
    # Create highest qualification variable
    hiqual_dv = str_to_lower(hiqual_dv),
    hiqual_dv = str_trim(hiqual_dv),
    hiqual = case_when(
      hiqual_dv %in% c("no qualification", "no qual") ~ "0_noqual",
      hiqual_dv %in% c("gcse etc") ~ "1_gcse",
      hiqual_dv %in% c("a level etc", "a-level etc") ~ "2_ALevel",
      hiqual_dv == "degree" ~ "3_undergrad",
      hiqual_dv %in% c("other higher", "other higher degree") ~ "4_higherEd",
      TRUE ~ NA_character_
    ),
    
    # Create binary educ variable
    GCSE = ifelse(hiqual %in% c("0_noqual", NA), 0, 1),
    ALevel = ifelse(hiqual %in% c("0_noqual", "1_gcse", NA), 0, 1),
    Undergrad = ifelse(hiqual %in% c("0_noqual", "1_gcse", "2_ALevel", NA), 0, 1),
    HigherEd = ifelse(hiqual %in% c("0_noqual", "1_gcse", "2_ALevel", "3_undergrad", NA), 0, 1),
    
    #Handling expryrs
    jbstat = str_to_lower(jbstat),
    jbstat = str_trim(jbstat),
    isWorking = case_when(
      jbstat %in% c("employed","self-employed","self employed", "paid employment(ft/pt)",
                    "unpaid, family business", "on furlough", "temporarily laid off/short term working",
                    "on apprenticeship", "govt training scheme","gvt trng scheme",
                    "on maternity leave","maternity leave", "on shared parental leave",
                    "on adoption leave") ~ 1,
      jbstat %in% c("ft studt, school","full-time student") ~ 0,
      TRUE ~ NA_real_
    ),
    expyrs = cumsum(isWorking),
    
    #Calculating log of wage
    lwage = log(fimnlabgrs_dv + 1), #TODO: change this later,
    
    #ROSLA instruments
    affected_2013 = as.integer(any(isWorking == 0 & year >= 2013, na.rm = TRUE)),
    affected_2015 = as.integer(any(isWorking == 0 & year >= 2015, na.rm = TRUE)),
    ROSLA2013 = if_else(year >= 2013, affected_2013, 0L),
    ROSLA2015 = if_else(year >= 2015, affected_2015, 0L),
    child = nchild_dv
  )

#Import NLW variable into the dataset
usoc_clean <- left_join(usoc_clean, macro, by = "year")

# FINAL DATASET
usoc_df <- usoc_clean %>%
  select(pidp, year, lwage, hiqual, GCSE, ALevel, Undergrad, HigherEd, expyrs, 
         NLW, unemp, ROSLA2013, ROSLA2015, isWorking, child)  %>%
  arrange(pidp, year) %>%
  group_by(pidp)
usoc_tsibble <- usoc_df %>% as_tsibble(key = pidp, index = year)
usoc_plm<- pdata.frame(usoc_df, index = c("pidp", "year"))
