usoc <- usoc %>%
  mutate(across(where(is.character), ~ str_trim(str_to_lower(.x))))


# ── 1. Pre-compute carry-forward columns (race, ability, momeduc) ──────────────
# first(col[!is.na(col) & ...]) inside group_by evaluates a filtered vector for
# every row. Doing it once per pidp with summarise + join is much faster.
# FIX B3: normalise each lookup column after summarise — these columns are
# created after the upfront across() call so they need their own normalisation.

race_lookup <- usoc %>%
  group_by(pidp) %>%
  summarise(
    race = first(
      race[!race %in% c(NA, "missing", "proxy", "refusal", "inapplicable")],
      default = NA_character_
    ),
    .groups = "drop"
  ) %>%
  mutate(race = str_trim(str_to_lower(race)))

ability_lookup <- usoc %>%
  group_by(pidp) %>%
  summarise(
    ability_raw = first(
      ability[!ability %in% c(NA, "missing", "proxy", "refusal", "inapplicable")],
      default = NA_character_
    ),
    .groups = "drop"
  ) %>%
  mutate(ability_raw = str_trim(str_to_lower(ability_raw)))

momeduc_lookup <- usoc %>%
  group_by(pidp) %>%
  summarise(
    momeduc_raw = first(
      momeduc[!momeduc %in% c(NA, "missing", "proxy", "refusal", "inapplicable")],
      default = NA_character_
    ),
    .groups = "drop"
  ) %>%
  mutate(momeduc_raw = str_trim(str_to_lower(momeduc_raw)))

# FIX B4: dplyr::select() to avoid namespace conflicts
usoc <- usoc %>%
  dplyr::select(-any_of(c("race", "ability", "momeduc"))) %>%
  left_join(race_lookup,    by = "pidp") %>%
  left_join(ability_lookup, by = "pidp") %>%
  left_join(momeduc_lookup, by = "pidp")


# ── 2. Main mutate (grouped) ───────────────────────────────────────────────────
usoc_clean <- usoc %>%
  mutate(
    pidp = as.integer(as.character(pidp)),
    year = as.integer(as.character(year))
  ) %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  
  # ── Block A: VOC flags only ─────────────────────────────────────────────────
  # VOC must be filled BEFORE hiqual uses it (FIX B1).
  mutate(
    birth_year = ifelse(is.na(birth_year), year - age, birth_year),
    
    appr = case_when(appr == "mentioned" ~ 1L, TRUE ~ NA_integer_),
    NVQ  = case_when(NVQ  == "mentioned" ~ 1L, TRUE ~ NA_integer_),
    ONC  = case_when(ONC  == "mentioned" ~ 1L, TRUE ~ NA_integer_),
    BTEC = case_when(BTEC == "mentioned" ~ 1L, TRUE ~ NA_integer_),
    VOC  = case_when(
      appr == 1 | NVQ == 1 | ONC == 1 | BTEC == 1 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  # Fill VOC now so it is available when hiqual is computed below
  fill(VOC, .direction = "down") %>%
  mutate(VOC = replace(VOC, is.na(VOC), 0L)) %>%
  
  # ── Block B: everything that depends on VOC ─────────────────────────────────
  mutate(
    # ── Highest qualification ──────────────────────────────────────────────────
    hiqual = case_when(
      hiqual_dv %in% c("no qualification", "no qual")                ~ "Noqual",
      hiqual_dv == "gcse etc"                                        ~ "GCSE",
      hiqual_dv %in% c("a level etc", "a-level etc") & VOC == 1     ~ "Vocational",
      hiqual_dv %in% c("a level etc", "a-level etc")                ~ "ALevel",
      hiqual_dv == "degree" & qfhigh_dv == "higher degree"          ~ "HigherDeg",
      hiqual_dv == "degree"                                          ~ "Bachelor",
      hiqual_dv %in% c("other higher", "other higher degree")        ~ "OtherDip",
      TRUE                                                           ~ NA_character_
    ),
    
    # ── Binary education dummies ───────────────────────────────────────────────
    GCSE      = case_when(hiqual == "Noqual" ~ 0L, TRUE ~ 1L),
    ALevel    = case_when(hiqual %in% c("ALevel", "Bachelor", "HigherDeg") ~ 1L, TRUE ~ 0L),
    Bachelor  = case_when(hiqual %in% c("Bachelor", "HigherDeg") ~ 1L, TRUE ~ 0L),
    HigherDeg = case_when(hiqual == "HigherDeg" ~ 1L, TRUE ~ 0L),
    OtherDip  = case_when(hiqual == "OtherDip"  ~ 1L, TRUE ~ NA_integer_),
    
    # ── Employment / experience ────────────────────────────────────────────────
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
    emp_lag = dplyr::lag(isWorking, n = 1),
    
    # ── Caring ─────────────────────────────────────────────────────────────────
    isCare = case_when(
      aidhh == "yes"                      ~ 1L,
      aidhh %in% c("no", "inapplicable") ~ 0L,
      TRUE                                ~ NA_integer_
    ),
    
    # ── Region ─────────────────────────────────────────────────────────────────
    gor_dv = str_replace_all(gor_dv, " ", "."),
    gor_dv = recode(
      gor_dv,
      "east.of.england"    = "east",
      "yorkshire.&.humber" = "yorkshire.and.the.humber"
    ),
    
    # ── Policy instruments ─────────────────────────────────────────────────────
    year16 = birth_year + 16,
    year18 = birth_year + 18,
    PGLoan2016   = as.integer(year >= 2016 & Bachelor == 1 & age < 60),
    home_bachfee = case_when(
      year18 < 1998                   ~ 0,
      year18 >= 1998 & year18 <= 2005 ~ 1000,
      year18 >= 2006 & year18 <= 2011 ~ 3000,
      year18 >= 2012 & year18 <= 2016 ~ 9000,
      year18 >= 2017 & year18 <= 2024 ~ 9250,
      year18 == 2025                  ~ 9535,
      year18 == 2026                  ~ 9795
    ),
    
    # ── Race (recode pre-joined, pre-normalised column) ────────────────────────
    race = case_when(
      str_detect(race, "white")                                      ~ "White",
      str_detect(race, "mixed")                                      ~ "Mixed",
      str_detect(race, "asian|indian|pakistani|bangladeshi|chinese") ~ "Asian",
      str_detect(race, "black|african|caribbean")                    ~ "Black",
      str_detect(race, "arab|other ethnic")                          ~ "Other",
      TRUE                                                           ~ NA_character_
    ),
    
    # ── Ability (recode pre-joined, pre-normalised column) ─────────────────────
    ability = case_when(
      ability_raw == "no item answered correctly"   ~ 0,
      ability_raw == "all items answered correctly" ~ 10,
      ability_raw %in% as.character(1:9)            ~ as.numeric(ability_raw),
      TRUE                                          ~ NA_real_
    ),
    
    # ── Sex ────────────────────────────────────────────────────────────────────
    sex = case_when(
      sex == "male"   ~ "male",
      sex == "female" ~ "female",
      TRUE            ~ "other"
    ),
    
    # ── Mother's education (recode pre-joined, pre-normalised column) ──────────
    momeduc = case_when(
      momeduc_raw %in% c(
        "never went to school",
        "she did not go to school at all"
      )                                                                ~ "No Schooling",
      momeduc_raw %in% c(
        "left school no quals",
        "she left school with no qualifications or certificates"
      )                                                                ~ "Left school no quals",
      momeduc_raw %in% c(
        "left sch w some qual",
        "she left school with some qualifications or certificates"
      )                                                                ~ "Left school with quals",
      str_detect(momeduc_raw, "further|post school")                   ~ "Further education",
      str_detect(momeduc_raw, "degree")                                ~ "Degree",
      TRUE                                                             ~ NA_character_
    ),
    
    numSib = case_when(
      numSib == "none in hh"         ~ 0,
      numSib %in% as.character(1:12) ~ as.numeric(numSib),
      TRUE                           ~ NA_real_
    )
    
  ) %>%
  
  # ── FIX B2: fill OtherDip immediately after it is computed ─────────────────
  fill(OtherDip, .direction = "down") %>%
  mutate(OtherDip = replace(OtherDip, is.na(OtherDip), 0L)) %>%
  
  ungroup() %>%
  
  # ── Apply factors AFTER ungroup() ──────────────────────────────────────────
  mutate(
    hiqual = factor(hiqual,
                    levels = c("Noqual", "GCSE", "ALevel", "Vocational",
                               "OtherDip", "Bachelor", "HigherDeg")),
    race    = factor(race,
                     levels = c("White", "Asian", "Black", "Mixed", "Other")),
    sex     = factor(sex,
                     levels = c("male", "female", "other")),
    momeduc = factor(momeduc,
                     levels = c("No Schooling", "Left school no quals",
                                "Left school with quals", "Further education",
                                "Degree"))
  )


# ── 5. Import external data and merge ──────────────────────────────────────────
regional_unemp    <- read_excel("External Data/regional.xlsx", sheet = "unemp")
regional_unicount <- read_excel("External Data/regional.xlsx", sheet = "unicount")
CPI18_data        <- read_excel("External Data/regional.xlsx", sheet = "CPI18")

regional_unemp_long <- regional_unemp %>%
  pivot_longer(-year16, names_to = "gor_dv", values_to = "reg_unemp16")

regional_unicount_long <- regional_unicount %>%
  pivot_longer(-year16, names_to = "gor_dv", values_to = "reg_unicount16")

# Chain all three joins in one pipeline instead of three separate assignments
usoc_clean <- usoc_clean %>%
  left_join(regional_unemp_long,    by = c("year16", "gor_dv")) %>%
  left_join(regional_unicount_long, by = c("year16", "gor_dv")) %>%
  left_join(CPI18_data,             by = "year18")


# ── 6. Minimum wage: vectorised join instead of mapply ─────────────────────────
# Original: mapply(get_real_mw, year, age) called an R function row-by-row.
# Replacement: assign each row to an age-band string, pivot mw_cpi to long
# format, then join — fully vectorised.

mw_cpi <- read_excel("External Data/mw_cpi.xlsx", sheet = "real_converted")

mw_long <- mw_cpi %>%
  dplyr::select(-CPI, -Kaitz) %>%
  pivot_longer(
    -year,
    names_to  = "age_band",
    values_to = "realMW"
  )

usoc_clean <- usoc_clean %>%
  mutate(
    age_band = case_when(
      age < 18  ~ "Under 18",
      age >= 25 ~ "25 and over",
      TRUE      ~ as.character(age)   # 18, 19, 20, 21, 22, 23, 24
    )
  ) %>%
  left_join(mw_long, by = c("year", "age_band")) %>%
  dplyr::select(-age_band) %>%
  mutate(
    CPI   = mw_cpi$CPI[match(year,   mw_cpi$year)],
    Kaitz = mw_cpi$Kaitz[match(year, mw_cpi$year)]
  )


# ── 7. Final dataset ───────────────────────────────────────────────────────────
usoc_df <- usoc_clean %>%
  mutate(
    lwage         = asinh(fimnlabgrs_dv / CPI * 100),
    real_hbachfee = home_bachfee / CPI18 * 100,
    home_bachfee  = factor(
      as.character(home_bachfee),
      levels = c("0", "1000", "3000", "9000", "9250", "9535", "9795")
    )
  ) %>%
  dplyr::select(
    pidp, year, age, birth_year, lwage, hiqual, GCSE, ALevel, VOC,
    Bachelor, HigherDeg, OtherDip, expyrs, expyrs2, Kaitz,
    reg_unemp16, ability, emp_lag, reg_unicount16, real_hbachfee, home_bachfee,
    isWorking, numChild, isCare, numSib, PGLoan2016, race, sex, gor_dv,
    momeduc, FTStudying
  ) %>%
  # England only — policy instruments differ in devolved nations;
  # no HE participation data for Scotland / Wales / NI / Channel Islands
  filter(!gor_dv %in% c("scotland", "northern.ireland", "wales", "channel.islands")) %>%
  mutate(
    gor_dv = factor(
      gor_dv,
      levels = c(
        "london", "east", "east.midlands", "north.east", "north.west",
        "south.east", "south.west", "west.midlands", "yorkshire.and.the.humber"
      )
    )
  )


# ── 8. Working sub-sample ──────────────────────────────────────────────────────
usoc_working <- usoc_df %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  filter(min(age) <= 23, lwage > 0) %>%
  ungroup()
