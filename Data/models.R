
# ── 1. Selection indicator ─────────────────────────────────────────────────
usoc_df <- usoc_df %>%
  ungroup() %>% 
  mutate(selected = as.integer(!is.na(isWorking) & isWorking == 1))

# ── 2. Probit selection equation (pooled; full sample) ────────────────────

# P(selected_it = 1) = Φ(x'_it γ)
# Regressors: education dummies, experience, aggregate controls, instruments
# Note: pooled probit ignores individual heterogeneity in selection.
#       FE probit is biased (incidental parameters); RE probit is an alternative.
usoc_df <- usoc_df %>% ungroup()
probit_sel <- glm(
  selected ~ NLW + child,
  # append Z_i and X_it controls here
  data   = usoc_df,
  family = binomial(link = "probit")
)

# Inverse Mills ratio: λ_it = φ(x'_it γ̂) / Φ(x'_it γ̂)
usoc_df <- usoc_df %>%
  mutate(
    xg  = predict(probit_sel, newdata = usoc_df, type = "link"),
    imr = dnorm(xg) / pnorm(xg)
  )

# ── 3. Restrict to workers; create lwage_{i,t-1} ──────────────────────────
# We ungroup first to ensure we aren't trapped in the (pidp, year) grouping
usoc_df <- usoc_df %>%
  ungroup() %>%
  arrange(pidp, year) %>%
  group_by(pidp) %>%
  mutate(
    lwage_lag  = dplyr::lag(lwage, n = 1), # t-1
    lwage_lag2 = dplyr::lag(lwage, n = 2)  # t-2
  ) %>%
  ungroup()

# Filter for the working sample
# Note: You now require at least 3 years of data for a row to stay in 
# the sample (Current year, t-1, and t-2)
working_df <- usoc_df %>%
  filter(isWorking == 1) %>%
  filter(!is.na(lwage_lag) & !is.na(lwage_lag2))

# ── 4. Factor encoding of hiqual ─────────────────────────────────────────

working_df <- working_df %>%
  mutate(
    hiqual_fct = factor(
      hiqual,
      levels = c("0_noqual", "1_gcse", "2_ALevel", "3_undergrad", "4_higherEd")  # None = base
    )
  )

# ── 5. TWFE-2SLS ──────────────────────────────────────────────────────────

# Structural equation:
#   lwage_it = α_i + δ_t + β hiqual_it + ρ expyrs_it + θ λ_it + x'_it π + ε_it
#
# Endogenous: hiqual_num, expyrs
# Instruments: lwage_lag (varies at i×t, survives year FE)
#
# !! ROSLA2013, ROSLA2015, unemp only vary by t → absorbed by year FE → dropped.
#    To use them, interact with birth-year cohort: e.g., ROSLA2013 × I(birthyear >= 1997)
#    Requires a birth year variable. Add those interaction terms to the instrument list below.

twfe_iv <- feols(
  lwage ~ imr |
    pidp + year |
    hiqual_fct + expyrs ~                    # 5 endogenous variables
    lwage_lag + lwage_lag2 + ROSLA2013 + ROSLA2015 + unemp,       # 3 instruments → under-identified by 2
  data    = working_df,
  cluster = ~pidp
)
# First-stage diagnostics + second-stage results
summary(twfe_iv, stage = 1:2)
summary(twfe_iv)
