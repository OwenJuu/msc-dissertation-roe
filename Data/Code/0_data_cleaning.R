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
    SCOTVEC = str_trim(str_to_lower(SCOTVEC)),
    SCOTVEC = case_when(
      SCOTVEC == "mentioned" ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    VOC = case_when(
      (appr == 1 | NVQ == 1 | ONC == 1 | BTEC == 1 | SCOTVEC == 1) ~ 1L,
      TRUE ~ NA_integer_
    ),
    VOC = zoo::na.locf(VOC, na.rm = FALSE),
    VOC = replace(VOC, is.na(VOC), 0L), # VOC = ever taken a vocational degree
    
    hiqual_dv  = str_trim(str_to_lower(hiqual_dv)),
    qfhigh_dv  = str_trim(str_to_lower(qfhigh_dv)),
    hiqual = case_when(
      hiqual_dv %in% c("no qualification", "no qual")           ~ "Noqual",
      hiqual_dv == "gcse etc"                                   ~ "GCSE",
      hiqual_dv %in% c("a level etc", "a-level etc") & VOC == 0 ~ "ALevel",
      hiqual_dv %in% c("a level etc", "a-level etc")            ~ "Vocational",
      hiqual_dv == "degree" & qfhigh_dv == "higher degree"      ~ "HigherDeg",
      hiqual_dv == "degree"                                     ~ "Bachelor",
      hiqual_dv %in% c("other higher", "other higher degree")   ~ "OtherDip",
      hiqual_dv %in% c("other qualification", "other qual")   ~ "OtherQual",
      TRUE                                                      ~ NA_character_
    ),

    # ── Binary education dummies ──────────────────────────────────────────
    # No qual < GCSE < ALevel < Bachelor < HigherDeg. OtherDip stands alone
    GCSE = if_else(hiqual != "Noqual", 1L, 0L),
    
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
    PGLoan2016  = as.integer(!region %in% c('wales', 'scotland', 'northern.ireland') 
                             & year >= 2016 & age >= 22 & age < 60),
    
    region18_raw = region[match(year18, year)],
    region17_raw = region[match(year16 + 1, year)],
    region16_raw = region[match(year16, year)],
    region16 = coalesce(region16_raw, region17_raw, region18_raw),
    region18 = coalesce(region18_raw, region17_raw, region16_raw),
    region22_raw = region[match(year22, year)],
    region21_raw = region[match(year21, year)],
    region20_raw = region[match(year22 - 2, year)],
    region19_raw = region[match(year22 - 3, year)],
    region22 = coalesce(region22_raw, region21_raw, region20_raw, region19_raw),
    
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
                               "OtherQual", "OtherDip", "Bachelor", "HigherDeg")),
    race = factor(race,
      levels = c("White", "Asian", "Black", "Mixed", "Other")),
    sex = factor(sex,
      levels = c("male", "female", "other")),
    momeduc = factor(momeduc,
                     levels = c("No Schooling", "Left school no quals",
                                "Left school with quals", "Further education",
                                "Degree"))
  ) 
