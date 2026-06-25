usoc_working <- usoc_working %>%
  mutate(
    ParentHigherDeg = as.integer(momeduc == "Degree"),
    )

# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~  GCSE + ALevel + VOC + Bachelor + HigherDeg + expyrs 
  + expyrs2 + sex + race | gor_dv,                           
  data    = usoc_working,
  cluster = ~pidp
)
summary(simple_linear)

## KEEP THIS CONSTANT. COPY PASTE AND CHANGE ELSEWHERE PLEASE!!!!!!
iv_panel <- feols(lwage ~ sex + race| gor_dv | 
                    ALevel + VOC + Bachelor + expyrs + expyrs2  
                  ~ PGLoan2016 + home_bachfee + reg_unemp16 + numSib,
      data = usoc_working,
      cluster = ~pidp)
summary(iv_panel, stage = 2)


## TEST NEW MODEL HERE
iv_panel <- feols(lwage ~ sex + race | gor_dv |
                    ALevel + VOC + Bachelor + expyrs + expyrs2  
                  ~ PGLoan2016 + home_bachfee + numSib + reg_unemp16 + momeduc,
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 2)

# Kaitz interaction term
model_np <- feols(lwage ~ sex + race + bs(Kaitz, 4)|  gor_dv | 
                ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
              + ALevel:bs(Kaitz, 4) + VOC:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) + HigherDeg:bs(Kaitz, 4)
              + expyrs:bs(Kaitz, 4) + expyrs2:bs(Kaitz, 4)
              ~ PGLoan2016 + home_bachfee + reg_unemp16  + emp_lag + numSib  
              + PGLoan2016:bs(Kaitz, 4) + home_bachfee:bs(Kaitz, 4) 
              + reg_unemp16:bs(Kaitz, 4) + emp_lag:bs(Kaitz, 4) + numSib:bs(Kaitz, 4), 
              data    = usoc_working,
              cluster = ~pidp)


summary(model_np)