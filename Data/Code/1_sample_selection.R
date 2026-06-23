# variable list
# - Endogenous: ALevel + Bachelor + HigherDeg + expyrs + expyrs2
# - Control: sex + race + momeduc + factor(birth_year)
# - Instruments: PGLoan2016 + home_bachfee + emp_lag + reg_unemp18 + reg_unicount18
# - Selection: numChild + isCare + lwage_lag

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
probit_sel <- feglm(
  isWorking ~ numChild + isCare + sex + race + momeduc + ability
  + PGLoan2016 + home_bachfee + emp_lag + reg_unemp18 + reg_unicount18, 
  data   = working_df,
  family = binomial(link = "probit")
)
summary(probit_sel)

# Inverse Mills ratio
working_df <- working_df %>%
  mutate(
    xg  = predict(probit_sel, newdata = working_df, type = "link"),
    imr = dnorm(xg) / pnorm(xg),
    imr = ifelse(is.infinite(imr) | is.nan(imr), NA, imr),
    imr = pmin(imr, quantile(imr, 0.99, na.rm = TRUE))
  )

working_df <- working_df %>%
  group_by(pidp) %>%
  filter(
    first(age) <= 18|(first(age) > 18 & first(age) <= 23 & first(FTStudying) == 1)
  ) %>%
  ungroup() %>%
  filter(isWorking == 1, lwage > 0)