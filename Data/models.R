# =============================================================================
# RETURNS TO EDUCATION & EXPERIENCE — PANEL ECONOMETRICS
# Dataset used per model:
#   plm(model="fd")  → usoc_plm   (panel object, no manual differencing needed)
#   feols() TWFE     → usoc_df    (fastest with standard dataframe)
#   att_gt() DiD     → usoc_df    (requires standard dataframe)
#   tsibble          → not used   (no package requires it specifically)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
packages <- c(
  "dplyr",                    # data wrangling
  "plm",                      # FD model
  "fixest",                   # TWFE
  "did",                      # Callaway & Sant'Anna staggered DiD
  "lmtest", "sandwich",       # cluster-robust SE for plm
  "ggplot2"                   # event-study plots
)

invisible(lapply(packages, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))


# =============================================================================
# SECTION 1 — FIRST DIFFERENCE MODEL
# Package: plm | Dataset: usoc_plm
# plm(model = "fd") differences all variables internally — no manual lag needed
# =============================================================================

fd_model <- plm(
  lwage ~ middleSchool + highSchool + undergrad + higherEd + expyrs + NLW,
  data   = usoc_plm,
  model  = "fd",
  effect = "individual"
)

fd_se <- coeftest(
  fd_model,
  vcov = vcovHC(fd_model, method = "arellano", cluster = "group")
)

cat("\n===== SECTION 1: First Difference (usoc_plm) =====\n")
print(fd_se)


# =============================================================================
# SECTION 2 — TWO-WAY FIXED EFFECTS (TWFE)
# Package: fixest | Dataset: usoc_df
#
# 2a: Time FE absorbs NLW entirely (NLW only varies by year, not individual).
#     Use this for clean education/experience return estimates.
# 2b: Individual linear trends replace time FE, breaking the collinearity so
#     NLW's coefficient (sigma) is separately identified.
# =============================================================================

# --- 2a: TWFE — education & experience returns ---
twfe_main <- feols(
  lwage ~ middleSchool + highSchool + undergrad + higherEd + expyrs |
    pidp + year,
  data    = usoc_df,
  cluster = ~pidp
)

# --- 2b: TWFE — with NLW explicitly identified ---
twfe_nlw <- feols(
  lwage ~ middleSchool + highSchool + undergrad + higherEd + expyrs + NLW |
    pidp + pidp[year],          # individual FE + individual-specific linear trends
  data    = usoc_df,
  cluster = ~pidp
)

cat("\n===== SECTION 2a: TWFE — Main (usoc_df) =====\n")
print(summary(twfe_main))
cat("\n===== SECTION 2b: TWFE — With NLW (usoc_df) =====\n")
print(summary(twfe_nlw))


# =============================================================================
# SECTION 3 — STAGGERED DiD (Callaway & Sant'Anna 2021)
# Package: did | Dataset: usoc_df
# Cohort = first year individual completes each education level (0 = never)
# =============================================================================

usoc_cs <- usoc_df |>
  arrange(pidp, year) |>
  group_by(pidp) |>
  mutate(
    g_highSchool = if_else(any(highSchool == 1), min(year[highSchool == 1]), 0L),
    g_undergrad  = if_else(any(undergrad  == 1), min(year[undergrad  == 1]), 0L),
    g_higherEd   = if_else(any(higherEd   == 1), min(year[higherEd   == 1]), 0L)
  ) |>
  ungroup()

# Helper: run att_gt + all aggregations
run_cs <- function(data, gname) {
  out <- att_gt(
    yname         = "lwage",
    gname         = gname,
    idname        = "pidp",
    tname         = "year",
    xformla       = ~expyrs + NLW,
    data          = data,
    control_group = "notyettreated",
    anticipation  = 0,
    base_period   = "universal"
  )
  list(
    att_gt   = out,
    simple   = aggte(out, type = "simple"),
    dynamic  = aggte(out, type = "dynamic"),
    calendar = aggte(out, type = "calendar")
  )
}

cs_highSchool <- run_cs(usoc_cs, "g_highSchool")
cs_undergrad  <- run_cs(usoc_cs, "g_undergrad")
cs_higherEd   <- run_cs(usoc_cs, "g_higherEd")

cat("\n===== SECTION 3: Staggered DiD — High School =====\n");  print(cs_highSchool$simple)
cat("\n===== SECTION 3: Staggered DiD — Undergraduate =====\n"); print(cs_undergrad$simple)
cat("\n===== SECTION 3: Staggered DiD — Higher Ed =====\n");     print(cs_higherEd$simple)

# Event-study plots
ggdid(cs_highSchool$dynamic) + labs(title = "Event Study: High School")   + theme_minimal()
ggdid(cs_undergrad$dynamic)  + labs(title = "Event Study: Undergraduate") + theme_minimal()
ggdid(cs_higherEd$dynamic)   + labs(title = "Event Study: Higher Ed")     + theme_minimal()


# =============================================================================
# SECTION 4 — NLW INTERACTIONS: Relative Returns (Education vs Experience)
# Package: fixest | Dataset: usoc_df
# Uses individual linear trends (same as 2b) to identify NLW separately.
# =============================================================================

twfe_nlw_interaction <- feols(
  lwage ~
    middleSchool + highSchool + undergrad + higherEd + expyrs + NLW +
    NLW:highSchool + NLW:undergrad + NLW:higherEd +   # does NLW compress education premium?
    NLW:expyrs |                                        # does NLW raise return to experience?
    pidp + pidp[year],
  data    = usoc_df,
  cluster = ~pidp
)

cat("\n===== SECTION 4: TWFE with NLW Interactions =====\n")
print(summary(twfe_nlw_interaction))


# =============================================================================
# SECTION 5 — MODEL COMPARISON TABLE (fixest::etable)
# =============================================================================

etable(
  list(
    "TWFE"            = twfe_main,
    "TWFE + NLW"      = twfe_nlw,
    "TWFE + NLW x Ed" = twfe_nlw_interaction
  ),
  digits   = 3,
  se.below = TRUE,
  title    = "Returns to Education & Experience"
)