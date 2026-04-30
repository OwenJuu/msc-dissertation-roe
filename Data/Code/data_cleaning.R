# CLEAN DATA
usoc_clean <- usoc %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    pidp = as.integer(pidp),
    year = as.integer(as.character(year)),
    
    # Fill in race from first non-missing, non-inapplicable value per person
    race = first(race[!race %in% c(NA, "inapplicable")]),
    
    # ── Highest qualification ─────────────────────────────────────────────
    hiqual_dv  = str_trim(str_to_lower(hiqual_dv)),
    qfhigh_dv  = str_trim(str_to_lower(qfhigh_dv)),
    hiqual = case_when(
      hiqual_dv %in% c("no qualification", "no qual")          ~ "Noqual",
      hiqual_dv == "gcse etc"                                  ~ "GCSE",
      hiqual_dv %in% c("a level etc", "a-level etc")           ~ "ALevel",
      hiqual_dv == "degree" & qfhigh_dv == "higher degree"     ~ "HigherDeg",
      hiqual_dv == "degree"                                    ~ "Undergrad",
      hiqual_dv %in% c("other higher", "other higher degree")  ~ "OtherHigher",
      TRUE                                                     ~ NA_character_
    ),
    
    # ── Binary education dummies ──────────────────────────────────────────
    GCSE       = ifelse(!is.na(hiqual) & !hiqual %in% c("Noqual"), 1L, 0L),
    ALevel     = ifelse(!is.na(hiqual) & !hiqual %in% c("Noqual", "GCSE"), 1L, 0L),
    Undergrad  = ifelse(!is.na(hiqual) & !hiqual %in% c("Noqual", "GCSE", "ALevel"), 1L, 0L),
    HigherDeg  = ifelse(!is.na(hiqual) & hiqual == "HigherDeg", 1L, 0L),
    OtherHigher = ifelse(!is.na(hiqual) & hiqual == "OtherHigher", 1L, 0L),
    
    # ── Employment / experience ───────────────────────────────────────────
    jbstat = str_trim(str_to_lower(jbstat)),
    isWorking = case_when(
      jbstat %in% c(
        "employed", "self-employed", "self employed",
        "paid employment(ft/pt)", "unpaid, family business",
        "on furlough", "temporarily laid off/short term working",
        "on apprenticeship", "govt training scheme", "gvt trng scheme",
        "on maternity leave", "maternity leave",
        "on shared parental leave", "on adoption leave"
      ) ~ 1L,
      TRUE ~ 0L
    ),
    FTStudying = case_when(
      jbstat %in% c("ft studt, school", "full-time student") ~ 1L,
      TRUE ~ 0L
    ),
    expyrs  = cumsum(isWorking),
    expyrs2 = expyrs^2,
    
    # ── Log wage ──────────────────────────────────────────────────────────
    lwage = asinh(fimnlabgrs_dv),
    
    # ── Caring ────────────────────────────────────────────────────────────
    aidhh = str_trim(str_to_lower(aidhh)),
    isCare = case_when(
      aidhh == "yes" ~ 1L,
      aidhh == "no"  ~ 0L,
      TRUE           ~ NA_integer_
    ),
    
    # ── Region (kept as character here; factor applied after ungroup) ─────
    gor_dv = str_trim(str_to_lower(gor_dv)),
    gor_dv = str_replace_all(gor_dv, " ", "."),
    gor_dv = recode(
      gor_dv,
      "east.of.england"      = "east",
      "yorkshire.&.humber"   = "yorkshire.and.the.humber"
    ),
    
    # ── Policy instruments ────────────────────────────────────────────────
    birth_year  = year - age,
    ROSLA2013   = as.integer(year >= 2013) * as.integer(birth_year == 1997),
    ROSLA2015   = as.integer(year >= 2015) * as.integer(birth_year >= 1998),
    PGLoan2016  = as.integer(year >= 2016 & age >= 21 & age <= 30),
    Fee2012     = as.integer(year >= 2012 & age >= 18 & age <= 20),
    
    # ── Race group (kept as character here; factor applied after ungroup) ─
    race_group = case_when(
      str_detect(race, "white")                                          ~ "White",
      str_detect(race, "mixed")                                          ~ "Mixed",
      str_detect(race, "asian|indian|pakistani|bangladeshi|chinese")     ~ "Asian",
      str_detect(race, "black|african|caribbean")                        ~ "Black",
      str_detect(race, "arab|other ethnic")                              ~ "Other",
      race %in% c("missing","refusal","proxy","inapplicable","don't know") ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    # ── Sex (kept as character here; factor applied after ungroup) ────────
    sex = str_trim(str_to_lower(sex)),
    sex = case_when(
      sex == "male"   ~ "male",
      sex == "female" ~ "female",
      TRUE            ~ "other"
    )
    
  ) %>%
  ungroup() %>%
  
  # ── Apply all factors AFTER ungroup() to prevent attribute loss ─────────
  mutate(
    hiqual = factor(
      hiqual,
      levels = c("Noqual", "GCSE", "ALevel", "Undergrad", "HigherDeg", "OtherHigher")
    ),
    race_group = factor(
      race_group,
      levels = c("White", "Asian", "Black", "Mixed", "Other")
    ),
    sex = factor(
      sex,
      levels = c("male", "female", "other")
    )
  ) 


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
  select(pidp, year, age, lwage, hiqual, GCSE, ALevel, Undergrad, HigherDeg, OtherHigher,
         expyrs, expyrs2, NLW, reg_unemp, ROSLA2013, ROSLA2015, isWorking, numChild,
         isCare, PGLoan2016, Fee2012, race_group, sex, gor_dv, FTStudying) %>%
  #Have to put the mutate here because left_join forces gor_dv to be character
  mutate(
    gor_dv = factor(
      gor_dv,
      levels = c(
        "london", "channel.islands", "east", "east.midlands",
        "north.east", "north.west", "northern.ireland", "scotland",
        "south.east", "south.west", "wales", "west.midlands",
        "yorkshire.and.the.humber"
      )
    )
  )

# Freeing some memory
rm(macro, regional_unemp, regional_unemp_long, usoc_clean)
gc()
