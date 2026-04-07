#Creating lag(wage)
working_df <- usoc_df %>%
  ungroup() %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    lwage_lag  = dplyr::lag(lwage, n = 1), # t-1
    lwage_lag2 = dplyr::lag(lwage, n = 2)  # t-2
  ) %>%
  ungroup()


# Probit selection model
probit_sel <- glm(
  isWorking ~ NLW + numChild + reg_unemp + isCare + lwage_lag,
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
    imr = pmin(imr, quantile(imr, 0.99, na.rm = TRUE))) %>% filter(!is.na(imr)
  )

# Filter for the working sample
# Note: You now require at least 3 years of data for a row to stay in 
# the sample (Current year, t-1, and t-2)
working_df <- working_df %>%
  filter(isWorking == 1) %>%
  filter(!is.na(lwage_lag) & !is.na(lwage_lag2)) %>%
  filter(!is.nan(lwage))

# ── TWFE-2SLS ──────────────────────────────────────────────────────────

# Structural equation:
#   lwage_it = α_i + δ_t + β hiqual_it + ρ expyrs_it + θ λ_it + x'_it π + ε_it
#
# Endogenous: GCSE + ALevel + Undergrad + HigherEd, expyrs
# Instruments: lwage_lag (varies at i×t, survives year FE)
#
# !! ROSLA2013, ROSLA2015, unemp only vary by t → absorbed by year FE → dropped.
#    To use them, interact with birth-year cohort: e.g., ROSLA2013 × I(birthyear >= 1997)
#    Requires a birth year variable. Add those interaction terms to the instrument list below.

twfe_iv <- feols(
  lwage ~ imr |
    pidp + year |
    ALevel + Undergrad + HigherEd + expyrs ~              # 5 endogenous variables
    lwage_lag + PG_Loan_Eligible + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp,  # 5 instruments 
  data    = working_df,
  cluster = ~pidp
)

