# CLEAN DATA
invalid_race <- c(
  NA,
  "missing",
  "proxy",
  "refusal",
  "inapplicable"
)

usoc_clean <- usoc %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    pidp = as.integer(pidp),
    year = as.integer(as.character(year)),
    
    # ── Highest qualification ─────────────────────────────────────────────
    hiqual_dv  = str_trim(str_to_lower(hiqual_dv)),
    qfhigh_dv  = str_trim(str_to_lower(qfhigh_dv)),
    hiqual = case_when(
      hiqual_dv %in% c("no qualification", "no qual")          ~ "Noqual",
      hiqual_dv == "gcse etc"                                  ~ "GCSE",
      hiqual_dv %in% c("a level etc", "a-level etc")           ~ "ALevel",
      hiqual_dv == "degree" & qfhigh_dv == "higher degree"     ~ "HigherDeg",
      hiqual_dv == "degree"                                    ~ "Bachelor",
      hiqual_dv %in% c("other higher", "other higher degree")  ~ "OtherDip",
      TRUE                                                     ~ NA_character_
    ),
    
    # ── Binary education dummies ──────────────────────────────────────────
    # No qual < GCSE < ALevel < Bachelor/OtherDip < HigherDeg
    GCSE        = ifelse(!is.na(hiqual) & !hiqual %in% c("Noqual"), 1L, 0L),
    ALevel      = ifelse(!is.na(hiqual) & !hiqual %in% c("Noqual", "GCSE"), 1L, 0L),
    Bachelor    = ifelse(!is.na(hiqual) &  hiqual %in% c("Bachelor", "HigherDeg"), 1L, 0L),
    HigherDeg   = ifelse(!is.na(hiqual) &  hiqual == "HigherDeg", 1L, 0L),
    OtherDip = ifelse(!is.na(hiqual) &  hiqual == "OtherDip", 1L, 0L),
    
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
    ROSLA2013   = as.integer(birth_year >= 1996 & birth_year < 1998),
    ROSLA2015   = as.integer(birth_year >= 1998),
    PGLoan2016  = as.integer(year >= 2016 & age >= 21 & age <= 30),
    Fee2012     = as.integer(year >= 2012 & age >= 18 & age <= 20),
    home_bachfee= case_when(
      birth_year + 18 < 1998 ~ "0",
      birth_year + 18 >= 1998 & birth_year + 18 <= 2005 ~ "1000",
      birth_year + 18 >= 2006 & birth_year + 18 <= 2011 ~ "3000",
      birth_year + 18 >= 2012 & birth_year + 18 <= 2016 ~ "9000",
      birth_year + 18 >= 2017 & birth_year + 18 <= 2024 ~ "9250"
    ),
    home_bachfee = factor(home_bachfee, levels = c("0", "1000", "3000", "9000", "9250")),
    
    
    # ── Race group (kept as character here; factor applied after ungroup) ─
    # Race is often recorded once and left missing in other waves.
    # For each individual (grouped by pidp), if a non-missing race value exists,
    # that value is propagated to all observations for the same pidp.
    race = str_trim(str_to_lower(race)),
    race = first(
      race[
        !is.na(race) & !race %in% invalid_race
      ],
      default = NA_character_
    ),
    race = case_when(
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
    hiqual = factor(hiqual,
                    levels = c("Noqual", "GCSE", "ALevel", "OtherDip", 
                               "Bachelor", "HigherDeg")),
    race = factor(
      race,
      levels = c("White", "Asian", "Black", "Mixed", "Other")
    ),
    sex = factor(
      sex,
      levels = c("male", "female", "other")
    )
  ) 


# IMPORT EXTERNAL DATA AND MERGE WITH THE ORIGINAL DATASET
## Regional unemployment
regional_unemp <- read.csv("External Data/regional_unemp.csv")
regional_hep <- read.csv(("External Data/regional_he_participation.csv"))
regional_unemp_long <- regional_unemp %>% #Regional unemployment rate
  pivot_longer(
    cols = -year,
    names_to = "gor_dv",
    values_to = "reg_unemp"
  )
usoc_clean <- left_join(usoc_clean, regional_unemp_long, by = c("year", "gor_dv"))

## Regional higher education participation
regional_hep_long <- regional_hep %>%  # Regional HE participation by 20
  pivot_longer(
    cols = -year,
    names_to = "gor_dv",
    values_to = "reg_hep"
  )
usoc_clean <- left_join(usoc_clean, regional_hep_long, by = c("year", "gor_dv"))

## Real minimum wage and CPI
mw_cpi <- read_excel("External Data/mw_cpi.xlsx", sheet = "real_converted")

get_real_mw <- function(year, age) {
  if (is.na(year) || is.na(age)) return(NA)
  if (!(year %in% mw_cpi$year)) return(NA)
  
  row <- mw_cpi[mw_cpi$year == year, ]
  
  col <- if (age < 18) {
    "Under 18"
  } else if (age >= 25) {
    "25 and over"
  } else {
    as.character(age)
  }
  
  val <- row[[col]]
  if (is.na(val)) return(NA)
  return(val)
}

# Apply to usoc_clean
usoc_clean$realMW <- mapply(get_real_mw, usoc_clean$year, usoc_clean$age)
usoc_clean$CPI <- mw_cpi$CPI[match(usoc_clean$year, mw_cpi$year)]

# FINAL DATASET
usoc_df <- usoc_clean %>%
  mutate(
    lwage = asinh(fimnlabgrs_dv/CPI*100)
  ) %>%
  dplyr::select(pidp, year, age, birth_year, lwage, hiqual, GCSE, ALevel, Bachelor, HigherDeg, OtherDip,
         expyrs, expyrs2, realMW, reg_unemp, home_bachfee, reg_hep, ROSLA2013, ROSLA2015, isWorking, numChild,
         isCare, PGLoan2016, Fee2012, race, sex, gor_dv, FTStudying) %>%


  #England only because education policy instruments is different for these four
  #as well there's no data on higher education participation
  filter(!gor_dv %in% c("scotland", "northern.ireland", "wales", "channel.islands")) %>%
  
  #Have to put the mutate here because left_join forces gor_dv to be character
  mutate(
    gor_dv = factor(
      gor_dv,
      levels = c(
        "london", "east", "east.midlands", "north.east", "north.west", 
        "south.east", "south.west", "west.midlands","yorkshire.and.the.humber"
      )
    )
  )

#Removing everything except for the OG "usoc" dataset, and freeing some memory
rm(list = setdiff(ls(), c("usoc","usoc_df", "mw_cpi", "regional_hep", "regional_unemp"))) 
gc()
