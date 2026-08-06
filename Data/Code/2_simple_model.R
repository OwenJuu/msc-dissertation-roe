usoc_working_filter <- usoc_working_complete %>%
  filter(race == "Asian")


# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~  ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) 
  + sex + race + numSib + OtherDip_exclude  | region + year + factor(birth_year),                           
  data    = usoc_working_complete,
  cluster = ~pidp
)
summary(simple_linear)

## BEST MODEL OVERALL
iv_panel <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current + OtherDip_exclude | 
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016, 
                  data = usoc_working_complete,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)
fsw(iv_panel)


## black SUBSAMPLE
iv_panel_hetero <- feols(lwage ~ as.factor(sex == "female") + numSib + runemp_current + rearn_current + OtherDip_exclude |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016, 
                  data = usoc_working_complete,
                  subset = ~race == "Black" ,
                  cluster = ~pidp)
summary(iv_panel_hetero, stage = 2)
#fsw(iv_panel_hetero)

post1999 <- usoc_working %>%
  filter(year >= 1999)

## FULL FLEDGE MODEL
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
                  data    = post1999,
                  cluster = ~pidp + year)
summary(model_np, stage = 2)
fsw(model_np)


## IV equivalence
library(ivreg)
library(lmtest)
library(sandwich)

iv_panel_ivreg <- ivreg::ivreg(
  lwage ~ sex + race + numSib + runemp_current + rearn_current +
    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) |
    sex + race + numSib + runemp_current + rearn_current +
    runemp16_devi + runemp18_devi + runemp22_devi +
    rearn16_devi + rearn18_devi + rearn22_devi +
    hbachfee18_real + runicount16 + PGLoan2016,
  data = usoc_working_complete,
  method = "LIML"
)

# Cluster-robust SEs by pidp, to match feols cluster = ~pidp
coeftest(iv_panel_ivreg, vcov = vcovCL(iv_panel_ivreg, cluster = ~pidp, type = "HC1"), df = Inf)
summary(iv_panel_ivreg, diagnostics = TRUE)