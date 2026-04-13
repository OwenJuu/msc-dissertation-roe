# CLEAN DATA
usoc_clean <- usoc %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    pidp = as.integer(pidp),
    year = as.integer(as.character(year)),
    race = first(race[!race %in% c(NA, "inapplicable")]), #Filling in all the missing race value
    
    # Create highest qualification variable
    hiqual_dv = str_to_lower(hiqual_dv),
    hiqual_dv = str_trim(hiqual_dv),
    hiqual = case_when(
      hiqual_dv %in% c("no qualification", "no qual") ~ "Noqual",
      hiqual_dv %in% c("gcse etc") ~ "GCSE",
      hiqual_dv %in% c("a level etc", "a-level etc") ~ "ALevel",
      hiqual_dv == "degree" ~ "Undergrad",
      hiqual_dv %in% c("other higher", "other higher degree") ~ "HigherEd",
      TRUE ~ NA_character_
    ),
    hiqual = factor(
      hiqual,
      levels = c("Noqual", "GCSE", "ALevel", "Undergrad", "HigherEd")  # Noqual = base
    ),
    
    # Create binary educ variable
    GCSE = ifelse(hiqual %in% c("Noqual", NA), 0, 1),
    ALevel = ifelse(hiqual %in% c("Noqual", "GCSE", NA), 0, 1),
    Undergrad = ifelse(hiqual %in% c("Noqual", "GCSE", "ALevel", NA), 0, 1),
    HigherEd = ifelse(hiqual %in% c("Noqual", "GCSE", "ALevel", "Undergrad", NA), 0, 1),
    
    #Handling expryrs
    jbstat = str_trim(str_to_lower(jbstat)),
    isWorking = case_when(
      jbstat %in% c("employed","self-employed","self employed", "paid employment(ft/pt)",
                    "unpaid, family business", "on furlough", "temporarily laid off/short term working",
                    "on apprenticeship", "govt training scheme","gvt trng scheme",
                    "on maternity leave","maternity leave", "on shared parental leave",
                    "on adoption leave") ~ 1,
      TRUE ~ 0
    ),
    expyrs = cumsum(isWorking),
    
    #Calculating log of wage
    lwage = log(fimnlabgrs_dv + 1), #TODO: change this later,
    
    aidhh = str_trim(str_to_lower(aidhh)),
    isCare = case_when(
      aidhh == "yes" ~ 1,
      aidhh == "no" ~ 0,
      TRUE ~ NA_real_ 
    ),
    
    #Standardize government region
    gor_dv = str_trim(str_to_lower(gor_dv)),
    gor_dv = gsub(" ", ".", gor_dv),
    gor_dv = recode(gor_dv,
                    "east.of.england" = "east",
                    "yorkshire.&.humber" = "yorkshire.and.the.humber"
    ),
    # Handling policies instrument.
    birth_year = year - age,
    ROSLA2013 = as.integer(year >= 2013) * as.integer(birth_year == 1997),
    ROSLA2015 = as.integer(year >= 2015) * as.integer(birth_year >= 1998),
    
    # Other policies
    PGLoan2016 = as.integer(year >= 2016 & age >= 21 & age <= 30),
    Fee2012 = as.integer(year >= 2012 & age >= 18 & age <= 20),
    
    #Race
    race_group = case_when(
      
      # White
      str_detect(race, "white") ~ "White",
      
      # Mixed
      str_detect(race, "mixed") ~ "Mixed",
      
      # Asian
      str_detect(race, "asian") |
        str_detect(race, "indian") |
        str_detect(race, "pakistani") |
        str_detect(race, "bangladeshi") |
        str_detect(race, "chinese") ~ "Asian",
      
      # Black
      str_detect(race, "black") |
        str_detect(race, "african") |
        str_detect(race, "caribbean") ~ "Black",
      
      # Other
      str_detect(race, "arab") |
        str_detect(race, "other ethnic") ~ "Other",
      
      # Missing / non-response
      race %in% c("missing", "refusal", "proxy", "inapplicable", "don't know") ~ NA_character_,
      
      TRUE ~ NA_character_),
  ) %>%
  ungroup() 


# IMPORT EXTERNAL DATA AND MERGE WITH THE ORIGINAL DATASET
macro <- read.csv("External Data/macro.csv")
regional_unemp <- read.csv("External Data/regional_unemp.csv")
regional_unemp_long <- regional_unemp %>%
  pivot_longer(
    cols = -year,
    names_to = "gor_dv",
    values_to = "reg_unemp"
  )

usoc_clean <- left_join(usoc_clean, macro, by = "year")
usoc_clean <- left_join(usoc_clean, regional_unemp_long, by = c("year", "gor_dv"))

# FINAL DATASET
usoc_df <- usoc_clean %>%
  select(pidp, year, age, lwage, hiqual, GCSE, ALevel, Undergrad, HigherEd, expyrs, 
         NLW, reg_unemp, ROSLA2013, ROSLA2015, isWorking, numChild, isCare, PGLoan2016, Fee2012, race_group)

# Freeing some memory
rm(macro, regional_unemp, regional_unemp_long, usoc_clean)
gc()
