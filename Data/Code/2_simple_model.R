# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~  GCSE + ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) 
  + sex + race + numSib + OtherDip_exclude  | region + year + factor(birth_year),                           
  data    = usoc_working_complete,
  cluster = ~pidp
)
summary(simple_linear)

## ORIGINAL OVERALL
iv_panel <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current 
                  + OtherDip_exclude | 
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016, 
                  data = usoc_working_complete,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)
fsw(iv_panel)

## HETEROGENEITY
usoc_working_filter <- usoc_working_complete %>%
  filter(race == "Mixed")

iv_panel_hetero <- feols(lwage ~ as.factor(sex == "female") + numSib + runemp_current + rearn_current + OtherDip_exclude |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016, 
                  data = usoc_working_filter,
                  cluster = ~pidp)
summary(iv_panel_hetero, stage = 1)
fsw(iv_panel_hetero)


## BSPLINE OF KAITZ INDEX: FULL FLEDGE MODEL
post2010 <- usoc_working_complete %>%
  filter(year >= 2010)

model_np <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current + OtherDip_exclude 
                  + bs(Kaitz, 4) |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) +
                    bs(Kaitz, 4):ALevel + VOC:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) +
                    HigherDeg:bs(Kaitz, 4) + bs(Kaitz, 4):sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016 +
                    runemp16_devi:bs(Kaitz, 4) + runemp18_devi:bs(Kaitz, 4) + runemp22_devi:bs(Kaitz, 4) +
                    rearn16_devi:bs(Kaitz, 4) + rearn18_devi:bs(Kaitz, 4) + rearn22_devi:bs(Kaitz, 4) +
                    hbachfee18_real:bs(Kaitz, 4) + runicount16:bs(Kaitz, 4) + PGLoan2016:bs(Kaitz, 4),  
                  data    = post2010,
                  cluster = ~pidp + year)
summary(model_np, stage = 2)
fsw(model_np)
