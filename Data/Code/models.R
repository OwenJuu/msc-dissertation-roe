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
  # append Z_i and X_it controls here
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

# TWFE-2SLS

twfe_iv <- feols(
  lwage ~ imr |
    pidp + year  |
    GCSE + ALevel + Undergrad + expyrs + expyrs2  ~
    lwage_lag + PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp,
  data    = working_df,
  cluster = ~pidp
)

summary(twfe_iv, stage = 1:2)

## Pooled IV
pooled_iv <- feols(
  lwage ~ imr + sex + race_group + NLW | year |
    ALevel + Undergrad + HigherEd + expyrs +  expyrs2~
    PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp,
  
  data = working_df,
  cluster = ~pidp
)

summary(pooled_iv, stage = 1:2)

## Pooled IV
ols <- feols(
  lwage ~ imr + sex + race_group + NLW + ALevel + Undergrad + 
    HigherEd + expyrs +  expyrs2 | year,
  
  data = working_df,
  cluster = ~pidp
)

summary(ols, stage = 1:2)

