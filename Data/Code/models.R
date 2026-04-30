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
changed_ids <- c(435465893, 479849485, 681428019, 750560899, 884767055, 
                 1292999611, 1497005059, 1565066939, 1581469370)
working_df <- working_df %>%
  group_by(pidp) %>%
  filter(
    first(age) < 18 |
      (first(age) >= 18 & first(age) <= 21 & first(FTStudying) == 1) # Keeping expyrs valid
  ) %>%
  ungroup() %>%
  filter(isWorking == 1, lwage > 0, !is.na(imr))

# TWFE-2SLS

## Simple OLS
ols <- feols(
  lwage ~ ALevel + Undergrad + HigherDeg + expyrs +  expyrs2 + 
    imr + sex + race_group + gor_dv + NLW ,
  
  data = working_df,
  cluster = ~pidp
)

summary(ols, stage = 2)

## Pooled IV
pooled_iv <- feols(
  lwage ~ imr + sex + race_group + gor_dv | year |
    ALevel + Undergrad + HigherDeg + expyrs + expyrs2~
    PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp + emp_lag,
  
  data = working_df,
  cluster = ~pidp
)

summary(pooled_iv, stage = 2)
