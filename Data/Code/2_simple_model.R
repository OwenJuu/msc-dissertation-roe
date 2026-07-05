usoc_working <- usoc_working %>%
  mutate(
    ParentHigherDeg = as.integer(momeduc == "Degree"),
    momQual = case_when(
      is.na(momeduc) ~ NA_integer_,
      momeduc %in% c("Further education", "Degree") ~ 1L,
      TRUE ~ 0L
    ),
    home_bachfeef = factor(
      as.character(home_bachfee),
      levels = c("0", "1000", "3000", "9000", "9250", "9535", "9795")),
    post1976 = as.numeric(birth_year >= 1976),
    post1974 = as.numeric(birth_year >= 1974),
    isWhite = as.numeric(race == "White")
    )

# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~  ALevel + VOC + Bachelor + HigherDeg + expyrs 
  + expyrs2 + sex + race | region,                           
  data    = usoc_working,
  cluster = ~pidp
)
summary(simple_linear)

## BEST MODEL OVERALL
iv_panel <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current |
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    real_hbachfee + runicount16 + PGLoan2016, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

post1999 <- usoc_working %>%
  filter(year >= 1999)

model_np <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current 
                   |
                    ALevel:bs(Kaitz, 4) + VOC:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) + HigherDeg:bs(Kaitz, 4) +
                    expyrs:bs(Kaitz, 4) + expyrs2:bs(Kaitz, 4) 
                  ~ 
                    runemp16_devi:bs(Kaitz, 4) + runemp18_devi:bs(Kaitz, 4) + runemp22_devi:bs(Kaitz, 4) +
                    rearn16_devi:bs(Kaitz, 4) + rearn18_devi:bs(Kaitz, 4) + rearn22_devi:bs(Kaitz, 4) +
                    real_hbachfee:bs(Kaitz, 4) + runicount16:bs(Kaitz, 4) + PGLoan2016:bs(Kaitz, 4),  
                  data    = post1999,
                  cluster = ~pidp)
summary(model_np)