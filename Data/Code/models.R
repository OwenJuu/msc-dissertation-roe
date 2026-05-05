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
  isWorking ~ lwage_lag + reg_unemp + NLW + numChild + isCare,
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

working_df <- working_df %>%
  group_by(pidp) %>%
  filter(
    first(age) < 18 |
      (first(age) >= 18 & first(age) <= 21 & first(FTStudying) == 1)
  ) %>%
  ungroup() %>%
  filter(isWorking == 1, lwage > 0, !is.na(imr))



# sanity <- glm(
#   ALevel ~ ROSLA2013 + ROSLA2015 ,
#   data   = working_df,
#   family = binomial(link = "probit")
# )
# summary(sanity)
# 
# 
# ## Simple OLS
# ols <- feols(
#   lwage ~ ALevel + Bachelor + HigherDeg + expyrs + expyrs2 + 
#     imr + sex + race + gor_dv + NLW ,
#   
#   data = working_df,
#   cluster = ~pidp
# )
# 
# summary(ols)
# 
# ## Year FE IV
# pooled_iv <- feols(
#   lwage ~ imr + sex + race + gor_dv | year |
#     ALevel + Bachelor + HigherDeg + expyrs + expyrs2~
#     PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp + emp_lag,
#   
#   data = working_df,
#   cluster = ~pidp
# )
# 
# summary(pooled_iv, stage = 1:2)
# 
# 
# ## Year FE IV
# #Serious identification issue. If someone is working on NLW
# pooled_NLW_iv <- feols(
#     lwage ~ imr + sex + race + gor_dv + NLW +
#     NLW:ALevel + NLW:Bachelor + NLW:HigherDeg +     # OLS — NLW interactions
#     NLW:expyrs + NLW:expyrs2 |                       # OLS — NLW interactions
#     ALevel + Bachelor + HigherDeg + expyrs + expyrs2  # IV — qualifications only
#   ~
#     PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp + emp_lag,
#   data    = working_df,
#   cluster = ~pidp
# )
# 
# summary(pooled_NLW_iv, stage = 2)
