usoc_clean <- usoc %>%
  mutate(
    pidp = as.integer(as.character(pidp)),
    year = as.integer(as.character(year))) %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    birth_year = ifelse(is.na(birth_year), year - age, birth_year),
    
    # ── Highest qualification ─────────────────────────────────────────────
    # Creating vocational study variable.
    appr  = str_trim(str_to_lower(appr)),
    appr = case_when(
      appr == "mentioned" ~ 1L,
      TRUE ~ NA_integer_
    ),
    NVQ  = str_trim(str_to_lower(NVQ)),
    NVQ = case_when(
      NVQ == "mentioned" ~ 1L,
      TRUE ~ NA_integer_
    ),
    ONC  = str_trim(str_to_lower(ONC)),
    ONC = case_when(
      ONC == "mentioned" ~ 1L,
      TRUE ~ NA_integer_
    ),
    BTEC = str_trim(str_to_lower(BTEC)),
    BTEC = case_when(
      BTEC == "mentioned" ~ 1L,
      TRUE ~ NA_integer_
    ),
    VOC = case_when(
      (appr == 1 | NVQ == 1 | ONC == 1 | BTEC == 1) ~ 1L,
      TRUE ~ NA_integer_
    ),
    VOC = zoo::na.locf(VOC, na.rm = FALSE),
    VOC = replace(VOC, is.na(VOC), 0L),
    
    hiqual_dv  = str_trim(str_to_lower(hiqual_dv)),
    qfhigh_dv  = str_trim(str_to_lower(qfhigh_dv)),
    hiqual = case_when(
      hiqual_dv %in% c("no qualification", "no qual")           ~ "Noqual",
      hiqual_dv == "gcse etc"                                   ~ "GCSE",
      hiqual_dv %in% c("a level etc", "a-level etc") & VOC == 1 ~ "Vocational",
      hiqual_dv %in% c("a level etc", "a-level etc")            ~ "ALevel",
      hiqual_dv == "degree" & qfhigh_dv == "higher degree"      ~ "HigherDeg",
      hiqual_dv == "degree"                                     ~ "Bachelor",
      hiqual_dv %in% c("other higher", "other higher degree")   ~ "OtherDip",
      TRUE                                                      ~ NA_character_
    ),

    # ── Binary education dummies ──────────────────────────────────────────
    # No qual < GCSE < ALevel < Bachelor < HigherDeg. OtherDip stands alone
    GCSE = case_when(hiqual == "GCSE" ~ 1L, TRUE ~ NA_integer_),
    GCSE = zoo::na.locf(GCSE, na.rm = FALSE),
    GCSE = replace(GCSE, is.na(GCSE), 0L),
    
    ALevel = case_when(hiqual == "ALevel" ~ 1L, TRUE ~ NA_integer_),
    ALevel = zoo::na.locf(ALevel, na.rm = FALSE),
    ALevel = replace(ALevel, is.na(ALevel), 0L),
    
    Bachelor = case_when(hiqual == "Bachelor" ~ 1L, TRUE ~ NA_integer_),
    Bachelor = zoo::na.locf(Bachelor, na.rm = FALSE),
    Bachelor = replace(Bachelor, is.na(Bachelor), 0L),
    
    HigherDeg = case_when(hiqual == "HigherDeg" ~ 1L, TRUE ~ NA_integer_),
    HigherDeg = zoo::na.locf(HigherDeg, na.rm = FALSE),
    HigherDeg = replace(HigherDeg, is.na(HigherDeg), 0L),
    
    OtherDip = case_when(hiqual == "OtherDip" ~ 1L, TRUE ~ NA_integer_),
    OtherDip = zoo::na.locf(OtherDip, na.rm = FALSE),
    OtherDip = replace(OtherDip, is.na(OtherDip), 0L),
    
    # ── Employment / experience ───────────────────────────────────────────
    jbstat = str_trim(str_to_lower(jbstat)),
    isWorkingFT = case_when(
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
    expyrs  = cumsum(isWorkingFT),
    expyrs2 = expyrs^2,
    emp_lag = dplyr::lag(isWorkingFT, n = 1),
    
    # ── Caring ────────────────────────────────────────────────────────────
    aidhh = str_trim(str_to_lower(aidhh)),
    isCare = case_when(
      aidhh == "yes" ~ 1L,
      aidhh %in% c("no", "inapplicable") ~ 0L,
      TRUE           ~ NA_integer_
    ),
    
    # ── Region (kept as character here; factor applied after ungroup) ─────
    region = str_trim(str_to_lower(gor_dv)),
    region = str_replace_all(region, " ", "."),
    region = recode(
      region,
      "east.of.england"      = "east",
      "yorkshire.&.humber"   = "yorkshire.and.the.humber"
    ),
    
    # ── Policy instruments ────────────────────────────────────────────────
    year16 = birth_year + 16, #Year at 16 years old
    year18 = birth_year + 18,
    year21 = birth_year + 21,
    year22 = birth_year + 22,
    PGLoan2016  = as.integer(year >= 2016 & age >= 22 & age < 60),
    home_bachfee = case_when( #Home bachelor tuition fee at 18 y/o
      (year18 < 1998) ~ 0,
      (year18 >= 1998 & year18 <= 2005) ~ 1000,
      (year18 >= 2006 & year18 <= 2011) ~ 3000,
      (year18 >= 2012 & year18 <= 2016) ~ 9000,
      (year18 >= 2017 & year18 <= 2024) ~ 9250,
      (year18 == 2025) ~ 9535,
    ),
    
    region18_raw = region[match(year18, year)],
    region17_raw = region[match(year16 + 1, year)],
    region16_raw = region[match(year16, year)],
    region16 = coalesce(region16_raw, region17_raw, region18_raw),
    region22_raw = region[match(year22, year)],
    region21_raw = region[match(year21, year)],
    region20_raw = region[match(year22 - 2, year)],
    region19_raw = region[match(year22 - 3, year)],
    region22 = coalesce(region22_raw, region21_raw, region20_raw, region19_raw),
    region21 = coalesce(region21_raw, region20_raw, region19_raw, region22_raw),
    
    # ── Race group (kept as character here; factor applied after ungroup) ─
    # Race is often recorded once and left missing in other waves.
    # For each individual (grouped by pidp), if a non-missing race value exists,
    # that value is propagated to all observations for the same pidp.
    race = str_trim(str_to_lower(race)),
    race = first(
      race[
        !is.na(race) & !race %in% c(NA, "missing", "proxy", "refusal","inapplicable")
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
    
    ability = str_trim(str_to_lower(ability)),
    ability = first(
      ability[
        !is.na(ability) & !ability %in% c("missing", "proxy", "refusal","inapplicable")
      ],
      default = NA_character_
    ),
    ability = case_when(
      ability == "no item answered correctly" ~ 0,
      ability == "all items answered correctly" ~ 10,
      ability %in% as.character(1:9) ~ as.numeric(as.character(ability)),
      TRUE ~ NA_integer_
    ),
    
    # ── Sex (kept as character here; factor applied after ungroup) ────────
    sex = str_trim(str_to_lower(sex)),
    sex = case_when(
      sex == "male"   ~ "male",
      sex == "female" ~ "female",
      TRUE            ~ "other"
    ),
    
    momeduc = str_trim(str_to_lower(momeduc)),
    momeduc = first(
      momeduc[!is.na(momeduc) & !momeduc %in% c("missing","proxy","refusal",
                                                "inapplicable","don't know",
                                                "only available for iemb",
                                                "not available for iemb"
      )],
      default = NA_character_
    ),
    momeduc = case_when(
      momeduc %in% c("never went to school", "she did not go to school at all") ~ "No Schooling",
      momeduc %in% c("left school no quals", "she left school with no qualifications or certificates") ~ "Left school no quals",
      momeduc %in% c("left sch w some qual", "she left school with some qualifications or certificates") ~ "Left school with quals",
      str_detect(momeduc, "further")|str_detect(momeduc, "post school") ~ "Further education",
      str_detect(momeduc, "degree") ~ "Degree",
      TRUE ~ NA_character_
    ),
    
    numSib = case_when(
      numSib == "none in hh" ~ 0,
      numSib %in% as.character(1:12) ~ as.numeric(as.character(numSib)),
      TRUE ~ NA_integer_
    )
    
  ) %>%
  ungroup() %>%
  
  # ── Apply all factors AFTER ungroup() to prevent attribute loss ─────────
  mutate(
    hiqual = factor(hiqual,
                    levels = c("Noqual", "GCSE", "ALevel", "Vocational", 
                               "OtherDip", "Bachelor", "HigherDeg")),
    race = factor(race,
      levels = c("White", "Asian", "Black", "Mixed", "Other")),
    sex = factor(sex,
      levels = c("male", "female", "other")),
    momeduc = factor(momeduc,
                     levels = c("No Schooling", "Left school no quals",
                                "Left school with quals", "Further education",
                                "Degree"))
  ) 

# IMPORT EXTERNAL DATA AND MERGE WITH THE ORIGINAL DATASET
## Regional unemployment and unicount at 18
regional_unemp <- read_excel("External Data/regional.xlsx", sheet = "unemp_devi")
regional_unemp_long <- regional_unemp %>% #Regional unemployment rate
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "reg_unemp"
  )

regional_unicount <- read_excel("External Data/regional.xlsx", sheet = "unicount")
regional_unicount_long <- regional_unicount %>%
  pivot_longer(
    cols = -year,
    names_to = "region",
    values_to = "reg_unicount16"
  )

CPI18 <- read_excel("External Data/regional.xlsx", sheet = "CPI18")

usoc_working <- usoc_clean %>%
  left_join(regional_unemp_long, by = c("year16" = "year", "region16" = "region")) %>%
  dplyr::rename(reg_unemp16 = reg_unemp) %>%
  
  left_join(regional_unemp_long, by = c("year21" = "year", "region21" = "region")) %>%
  dplyr::rename(reg_unemp21 = reg_unemp) %>%
  
  left_join(regional_unemp_long, by = c("year22" = "year", "region22" = "region")) %>%
  dplyr::rename(reg_unemp22 = reg_unemp) %>%
  
  left_join(regional_unicount_long, by = c("year16" = "year", "region16" = "region")) %>%
  
  left_join(CPI18, by = c("year18" = "year"))%>%
  dplyr::rename(CPI18 = CPI)  %>%

  left_join(CPI18, by = c("year"))
  

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
usoc_working$realMW <- mapply(get_real_mw, usoc_clean$year, usoc_clean$age)
usoc_working$Kaitz <- mw_cpi$Kaitz[match(usoc_clean$year, mw_cpi$year)]

# FINAL DATASET
usoc_working <- usoc_working %>%
  mutate(
    lwage = log(fimnlabgrs_dv/CPI*100),
    real_hbachfee = (home_bachfee/CPI18*100),
  ) %>%
  dplyr::select(pidp, year, age, birth_year, lwage, hiqual, GCSE, ALevel, VOC, 
                Bachelor, HigherDeg, OtherDip, expyrs, expyrs2, Kaitz, reg_unemp16, 
                reg_unemp21, reg_unemp22, ability, emp_lag, reg_unicount16, 
                real_hbachfee, home_bachfee, numChild, isCare, numSib, 
                PGLoan2016, race, sex, region, momeduc, isWorkingFT) %>%


  #England only because education policy instruments is different for these four
  #as well there's no data on higher education participation
  filter(!region %in% c("scotland", "northern.ireland", "wales", "channel.islands")) %>%
  
  #Have to put the mutate here because left_join forces region to be character
  mutate(
    region = factor(
      region,
      levels = c(
        "london", "east", "east.midlands", "north.east", "north.west", 
        "south.east", "south.west", "west.midlands","yorkshire.and.the.humber"
      )
    )
  )

usoc_working <- usoc_working %>%
  ungroup() %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  filter(min(age) <= 23, isWorkingFT == 1, lwage > 0) %>%
  ungroup()
