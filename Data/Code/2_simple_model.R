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
    econ22  = reg_unemp22 - reg_unemp21,
    )

# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~  ALevel + VOC + Bachelor + HigherDeg + expyrs 
  + expyrs2 + sex + race | region,                           
  data    = usoc_working,
  cluster = ~pidp
)
summary(simple_linear)

## KEEP THIS CONSTANT. COPY PASTE AND CHANGE ELSEWHERE PLEASE!!!!!!
iv_panel <- feols(lwage ~ sex + race | region |
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016*reg_unemp22 + real_hbachfee*reg_unemp16
                  + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

## SECOND BEST MODEL
iv_panel <- feols(lwage ~ sex + race | region + year |
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016 + reg_unemp22 + real_hbachfee * reg_unemp16
                  + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

## THIRD BEST MODEL
iv_panel <- feols(lwage ~ sex + race | region + year |
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016 + reg_unemp22 + real_hbachfee + reg_unemp16 
                  * log(reg_unicount16) + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)


## TEST NEW MODEL HERE
iv_panel <- feols(lwage ~ sex + race | region + year |
                    ALevel + VOC + Bachelor + HigherDeg + expyrs + expyrs2
                  ~ PGLoan2016 + reg_unemp22 + real_hbachfee + reg_unemp16 
                  * log(reg_unicount16) + numSib, 
                  data = usoc_working,
                  cluster = ~pidp)
summary(iv_panel, stage = 1:2)

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