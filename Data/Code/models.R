#Creating lag(wage)
working_df <- usoc_df

working_df <- usoc_df %>%
  ungroup() %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    lwage_lag  = dplyr::lag(lwage, n = 1), # t-1
    emp_lag = dplyr::lag(isWorking, n = 1)
  ) %>%
  ungroup()

working_df$race_group <- factor(
  working_df$race_group,
  levels = c("White", "Asian", "Black", "Mixed", "Other")
)

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
    imr = pmin(imr, quantile(imr, 0.99, na.rm = TRUE))) %>% filter(!is.na(imr)
  )

# Filter for the working sample
working_df <- working_df %>%
  group_by(pidp) %>%
  filter(
    first(age) < 18 |
      (first(age) >= 18 & first(age) <= 21 & first(FTStudying) == 1) # Keeping expyrs valid
  ) %>%
  ungroup() %>%
  filter(isWorking == 1, lwage > 0)

# ── TWFE-2SLS ──────────────────────────────────────────────────────────

# Structural equation:
#   lwage_it = α_i + δ_t + β hiqual_it + ρ expyrs_it + θ λ_it + x'_it π + ε_it
#
# Endogenous: GCSE + ALevel + Undergrad + HigherEd, expyrs
# Instruments: lwage_lag, PGLoan2016, Fee2012, ROSLA2013, ROSLA2015, reg_unemp

twfe_iv <- feols(
  lwage ~ imr |
    pidp + year|
    ALevel + Undergrad + HigherEd + expyrs + expyrs2~              # 4 endogenous variables
    lwage_lag + PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp,  # 6 instruments 
  data    = working_df,
  cluster = ~pidp
)
summary(twfe_iv, stage = 1:2)


twfe_iv_reduced <- feols(
  lwage ~ imr |
    pidp + year|
    ALevel + Undergrad + expyrs + expyrs2~              # 4 endogenous variables
    lwage_lag + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp,  # 6 instruments 
  data    = working_df,
  cluster = ~pidp
)
summary(twfe_iv_reduced, stage = 1:2)


#----------INTERACTION TERM
# NLW interaction terms (qualification × NLW)
working_df$NLW_GCSE      <- working_df$NLW * working_df$GCSE
working_df$NLW_ALevel    <- working_df$NLW * working_df$ALevel
working_df$NLW_Undergrad <- working_df$NLW * working_df$Undergrad
working_df$NLW_HigherEd  <- working_df$NLW * working_df$HigherEd
working_df$NLW_expyrs    <- working_df$NLW * working_df$expyrs
working_df$NLW_expyrs2    <- working_df$NLW * working_df$expyrs2

# Interacted instruments (for instrumented NLW x qual terms)
working_df$NLW_lwage_lag  <- working_df$NLW * working_df$lwage_lag
working_df$NLW_PGLoan2016 <- working_df$NLW * working_df$PGLoan2016
working_df$NLW_Fee2012    <- working_df$NLW * working_df$Fee2012
working_df$NLW_ROSLA2013  <- working_df$NLW * working_df$ROSLA2013
working_df$NLW_ROSLA2015  <- working_df$NLW * working_df$ROSLA2015
working_df$NLW_reg_unemp  <- working_df$NLW * working_df$reg_unemp

twfe_iv2 <- feols(
  lwage ~ imr + expyrs + expyrs^2 + NLW_expyrs + NLW_expyrs2|
    year |
    ALevel + Undergrad + HigherEd + 
    NLW_ALevel + NLW_Undergrad + NLW_HigherEd ~
    lwage_lag + PGLoan2016 + Fee2012 + ROSLA2013 + ROSLA2015 + reg_unemp +
    NLW_lwage_lag + NLW_PGLoan2016 + NLW_Fee2012 +
    NLW_ROSLA2013 + NLW_ROSLA2015 + NLW_reg_unemp,
  data    = working_df,
  cluster = ~pidp
)
summary(twfe_iv2, stage = 1:2)
