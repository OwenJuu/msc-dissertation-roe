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
  isWorking ~ PGLoan2016 + home_bachfee + emp_lag + reg_unemp 
  + lwage_lag + numChild + isCare + sex + race + gor_dv + factor(year),
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

clean_workingdf <- working_df[complete.cases(working_df), ]