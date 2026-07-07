vars <- c("lwage", "sex", "race", "numSib", "runemp_current", "rearn_current",
          "ALevel", "VOC", "Bachelor", "HigherDeg", "expyrs", "runemp16_devi",
          "runemp18_devi", "runemp22_devi", "rearn16_devi", "rearn18_devi",
          "rearn22_devi", "hbachfee18_real", "runicount16", "PGLoan2016", "pidp")

usoc_working_complete <- usoc_working[complete.cases(usoc_working[, vars]), ]
usoc_working_filter <- usoc_working_complete %>%
  filter(race == "Asian")


# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~  ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) + sex + race | region + year + factor(birth_year),                           
  data    = usoc_working_complete,
  cluster = ~pidp
)
summary(simple_linear)

## BEST MODEL OVERALL
iv_panel <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016, 
                  data = usoc_working_complete,
                  cluster = ~pidp)
summary(iv_panel, stage = 2)

iv_panel <- feols(lwage ~ as.numeric(sex == "female") + numSib + runemp_current + rearn_current |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016, 
                  data = usoc_working_filter,
                  cluster = ~pidp)
summary(iv_panel, stage = 2)

post1999 <- usoc_working %>%
  filter(year >= 1999)

model_np <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current 
                  + bs(Kaitz, 4) |
                    bs(Kaitz, 4):ALevel + VOC:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) + HigherDeg:bs(Kaitz, 4) +
                    bs(Kaitz, 4):sqrt(expyrs)
                  ~ 
                    runemp16_devi:bs(Kaitz, 4) + runemp18_devi:bs(Kaitz, 4) + runemp22_devi:bs(Kaitz, 4) +
                    rearn16_devi:bs(Kaitz, 4) + rearn18_devi:bs(Kaitz, 4) + rearn22_devi:bs(Kaitz, 4) +
                    hbachfee18_real:bs(Kaitz, 4) + runicount16:bs(Kaitz, 4) + PGLoan2016:bs(Kaitz, 4),  
                  data    = post1999,
                  cluster = ~pidp)
summary(model_np)

## IV equivalence
library(ivreg)
library(lmtest)
library(sandwich)

iv_panel_ivreg <- ivreg(
  lwage ~ sex + race + numSib + runemp_current + rearn_current +
    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) |
    sex + race + numSib + runemp_current + rearn_current +
    runemp16_devi + runemp18_devi + runemp22_devi +
    rearn16_devi + rearn18_devi + rearn22_devi +
    hbachfee18_real + runicount16 + PGLoan2016,
  data = usoc_working
)

# Cluster-robust SEs by pidp, to match feols cluster = ~pidp
coeftest(iv_panel_ivreg, vcov = vcovCL(iv_panel_ivreg, cluster = ~pidp, type = "HC1"), df = Inf)
summary(iv_panel_ivreg, diagnostics = TRUE)