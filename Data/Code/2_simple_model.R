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

## BEST FOR DEVI UNEMP
iv_panel <- feols(lwage ~ sex + race | region |
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016*runemp22_devi + real_hbachfee*runemp16_devi
                  + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

## BEST MODEL FOR RAW RUNEMP
iv_panel <- feols(lwage ~ sex + race + region16| region + year |
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016 + runemp22_raw + real_hbachfee + runemp16_raw 
                  * log(runicount16) + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)


## BEST IMPROVED MODEL FOR DEVI UNEMP (REJECT SARGAN)
iv_panel <- feols(lwage ~ sex + race + runemp_current| 
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016 + runemp22_devi + real_hbachfee + runemp16_devi 
                  * runicount16 + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

## Heckman model
iv_panel <- feols(lwage ~ sex + race + runemp_current + rearn_current 
                  + runemp16_avg + rearn16_avg + numSib + momeduc + factor(birth_year)| 
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ runemp16_raw + runemp18_raw + runemp22_raw + 
                    rearn16_raw + rearn18_raw + rearn22_raw + real_hbachfee + runicount16
                  + PGLoan2016, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

## test new model here
iv_panel <- feols(lwage ~ sex + race + runemp_current + runemp16_avg + factor(birth_year)| 
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016 + runemp22_raw + real_hbachfee + runemp16_raw 
                  + runicount16 + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

# Reduced form sanity check
sanity <- feols(lwage ~ region16 + year + PGLoan2016 + runemp22_devi + real_hbachfee + 
                  runemp16_devi + runicount16 + numSib, 
                data = usoc_working,
                cluster = ~pidp)
summary(sanity, stage = 2)



# Kaitz interaction term
model_np <- feols(lwage ~ sex + race + bs(Kaitz, 4)| region | 
                ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
              + ALevel:bs(Kaitz, 4) + VOC:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) 
              + HigherDeg:bs(Kaitz, 4)
              ~ PGLoan2016*reg_unemp22 + real_hbachfee*reg_unemp16 + numSib +  
              + PGLoan2016:bs(Kaitz, 4) + reg_unemp22:bs(Kaitz, 4) + PGLoan2016:reg_unemp22:bs(Kaitz, 4)
              + reg_unemp16:bs(Kaitz, 4) + real_hbachfee:bs(Kaitz, 4) + real_hbachfee:reg_unemp16:bs(Kaitz, 4) 
              + numSib:bs(Kaitz, 4), 
              data    = usoc_working,
              cluster = ~pidp)
summary(model_np)