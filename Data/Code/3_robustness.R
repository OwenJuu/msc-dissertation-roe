install.packages("ivreg2r", type = "win.binary")
library(ivreg2r)

## REDUCED FORM ESTIMATION
rf_model <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current + 
                 OtherDip_exclude + runemp16_devi + runemp18_devi + runemp22_devi + 
                 rearn16_devi + rearn18_devi + rearn22_devi + hbachfee18_real + 
                 runicount16 + PGLoan2016, 
                 data = usoc_working_complete,
                 cluster = ~pidp)
summary(rf_model)
wald(rf_model, keep = c("runemp16_devi", "runemp18_devi", "runemp22_devi",
                        "rearn16_devi", "rearn18_devi", "rearn22_devi",
                        "hbachfee18_real", "runicount16", "PGLoan2016"))

## USING LIML FOR MODEL 2
iv_panel_liml <- ivreg2(lwage ~ sex + race + numSib + runemp_current + rearn_current + OtherDip_exclude |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs)|
                    runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + PGLoan2016, 
                  data = usoc_working_complete,
                  vcov = "robust",
                  cluster = ~pidp,
                  method = "LIML")

summary(iv_panel_liml)

## REMOVE INSTRUMENTS FOR MODEL 3
# FULL MODEL
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

## REMOVED
## - PGLoan2016:bs(Kaitz, 4): 0.005
## - runicount16:bs(Kaitz, 4): 1.343e-5
## - hbachfee18_real:bs(Kaitz, 4): 4.834e-5
## - PGLoan2016: 0.0095
## - runicount16: 1.766e-4
## - hbachfee18_real: 0.003527
## - PGLoan2016 + PGLoan2016:bs(Kaitz, 4) = 0.007
## - PGLoan2016 + hbachfee18_real = 0.011
## - PGLoan2016 + hbachfee18_real + runicount16 = 0.007
## - PGLoan2016 + hbachfee18_real + runicount16:bs(Kaitz, 4) = 0.0586 (best)
## - PGLoan2016 + hbachfee18_real:bs(Kaitz, 4) + runicount16 = 0.0181
## - PGLoan2016 + hbachfee18_real:bs(Kaitz, 4) + runicount16:bs(Kaitz, 4) = 0.0782 (second best)
## - PGLoan2016:bs(Kaitz, 4) + hbachfee18_real + runicount16 = 0.0089
## - PGLoan2016:bs(Kaitz, 4) + hbachfee18_real:bs(Kaitz, 4) + runicount16 = 0.012
## - PGLoan2016:bs(Kaitz, 4) + hbachfee18_real + runicount16:bs(Kaitz, 4) = 0.032
## - PGLoan2016:bs(Kaitz, 4) + hbachfee18_real:bs(Kaitz, 4) + runicount16:bs(Kaitz, 4) = 0.032

model_np <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current + OtherDip_exclude 
                  + bs(Kaitz, 4) |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) +
                    bs(Kaitz, 4):ALevel + VOC:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) +
                    HigherDeg:bs(Kaitz, 4) + bs(Kaitz, 4):sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    runicount16 + 
                    runemp16_devi:bs(Kaitz, 4) + runemp18_devi:bs(Kaitz, 4) + runemp22_devi:bs(Kaitz, 4) +
                    rearn16_devi:bs(Kaitz, 4) + rearn18_devi:bs(Kaitz, 4) + rearn22_devi:bs(Kaitz, 4) +
                    hbachfee18_real:bs(Kaitz, 4) + PGLoan2016:bs(Kaitz, 4),  
                  data    = post1999,
                  cluster = ~pidp + year)
summary(model_np, stage = 2)


model_np <- feols(lwage ~ sex + race + numSib + runemp_current + rearn_current + OtherDip_exclude 
                  + bs(Kaitz, 4) |
                    ALevel + VOC + Bachelor + HigherDeg + sqrt(expyrs) +
                    bs(Kaitz, 4):ALevel + VOC:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) +
                    HigherDeg:bs(Kaitz, 4) + bs(Kaitz, 4):sqrt(expyrs)
                  ~ runemp16_devi + runemp18_devi + runemp22_devi 
                  + rearn16_devi + rearn18_devi + rearn22_devi + 
                    hbachfee18_real + runicount16 + 
                    runemp16_devi:bs(Kaitz, 4) + runemp18_devi:bs(Kaitz, 4) + runemp22_devi:bs(Kaitz, 4) +
                    rearn16_devi:bs(Kaitz, 4) + rearn18_devi:bs(Kaitz, 4) + rearn22_devi:bs(Kaitz, 4),  
                  data    = post1999,
                  cluster = ~pidp + year)
summary(model_np, stage = 2)
fsw(model_np)
