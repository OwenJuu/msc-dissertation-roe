#Creating lag(wage)
working_df <- usoc_df %>%
  ungroup() %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    lwage_lag  = dplyr::lag(lwage, n = 1), # t-1
    emp_lag = dplyr::lag(isWorking, n = 1),
  ) %>%
  ungroup()

# Probit selection model
probit_sel <- glm(
  isWorking ~ lwage_lag + PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp 
  + NLW + numChild + isCare,
  data   = working_df,
  family = binomial(link = "probit")
)
summary(probit_sel)

# Inverse Mills ratio: λ_it = φ(x'_it γ̂) / Φ(x'_it γ̂)
working_df <- working_df %>%
  mutate(
    xg  = predict(probit_sel, newdata = working_df, type = "link"),
    imr = dnorm(xg) / pnorm(xg),
    imr = ifelse(is.infinite(imr) | is.nan(imr), NA, imr),
    imr = pmin(imr, quantile(imr, 0.99, na.rm = TRUE))
  )

# Filter for the working sample
working_df <- working_df %>%
  group_by(pidp) %>%
  filter(
    first(age) < 18 |
      (first(age) >= 18 & first(age) <= 21 & first(FTStudying) == 1) # Keeping expyrs valid
  ) %>%
  ungroup() %>%
  filter(isWorking == 1, lwage > 0, !is.na(imr))


## Simple OLS
ols <- feols(
  lwage ~ ALevel + Undergrad + HigherDeg + expyrs + expyrs2 + 
    imr + sex + race + gor_dv + NLW ,
  
  data = working_df,
  cluster = ~pidp
)

summary(ols, stage = 2)

## Year FE IV
pooled_iv <- feols(
  lwage ~ imr + sex + race + gor_dv | year |
    ALevel + Undergrad + HigherDeg + expyrs + expyrs2~
    PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp + emp_lag,
  
  data = working_df,
  cluster = ~pidp
)

summary(pooled_iv, stage = 2)


## Year FE IV
pooled_NLW_iv <- feols(
  lwage ~ imr + sex + race + gor_dv + NLW |
    
    ALevel + Undergrad + HigherDeg + expyrs + expyrs2 +
    NLW:ALevel + NLW:Undergrad +  NLW:HigherDeg + NLW:expyrs + NLW:expyrs2
  ~
    PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp + emp_lag +
    NLW:PGLoan2016 + NLW:Fee2012 + NLW:ROSLA2013 + NLW:ROSLA2015 +
    NLW:reg_unemp + NLW:emp_lag,
  
  data = working_df,
  cluster = ~pidp
)

summary(pooled_NLW_iv, stage = 2)
