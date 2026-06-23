# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~ ALevel + Bachelor + HigherDeg + expyrs + expyrs2  
  + imr + sex + race | factor(birth_year) + momeduc ,                             
  data    = working_df,
  cluster = ~pidp
)

summary(simple_linear)

# IV Panel
iv_panel <- feols(lwage ~ imr + sex + race | gor_dv + momeduc |
                    ALevel + Bachelor + HigherDeg + expyrs + expyrs2 ~ PGLoan2016 
                  + home_bachfee + emp_lag + reg_unemp18 + reg_unicount18,                             
  data    = working_df,
  cluster = ~pidp
)

summary(iv_panel, stage = 2)


# Kaitz interaction term
model_np <- feols(lwage ~ imr + sex + race + bs(Kaitz, 4)| gor_dv + momeduc | 
                ALevel + Bachelor + HigherDeg + expyrs + expyrs2
              + ALevel:bs(Kaitz, 4) + Bachelor:bs(Kaitz, 4) + HigherDeg:bs(Kaitz, 4)
              ~ PGLoan2016 + home_bachfee + reg_unemp + emp_lag 
              + PGLoan2016:bs(Kaitz, 4) + home_bachfee:bs(Kaitz, 4) 
              + reg_unemp:bs(Kaitz, 4) + emp_lag:bs(Kaitz, 4),
              data    = clean_workingdf,
              cluster = ~pidp)


summary(model_np)