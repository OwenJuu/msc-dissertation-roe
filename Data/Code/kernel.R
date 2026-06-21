library(tidyverse)
library(splines)
library(fixest)

# ── 1. Setup ──────────────────────────────────────────────────────────────────
mw_grid <- seq(min(working_df$realMW, na.rm = TRUE),
               max(working_df$realMW, na.rm = TRUE),
               length.out = 100)

# Epanechnikov kernel
epan_kernel <- function(u) ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0)

# Bandwidth — start with Silverman's rule of thumb, then cross-validate
h <- 1.06 * sd(working_df$realMW, na.rm = TRUE) * 
  nrow(working_df)^(-1/5)

# ── 2. Local 2SLS at each MW point ───────────────────────────────────────────
local_results <- map_dfr(mw_grid, function(w0) {
  
  # Compute kernel weights
  u <- (working_df$realMW - w0) / h
  weights <- epan_kernel(u)
  
  # Drop observations with zero weight (outside kernel support)
  idx <- weights > 0
  df_local <- working_df[idx, ]
  w_local  <- weights[idx]
  
  # Skip if insufficient observations
  if (sum(idx) < 500) return(NULL)
  
  # Weighted 2SLS
  fit <- tryCatch(
    feols(lwage ~ imr + sex + race | gor_dv + factor(year) |
            ALevel + Bachelor + HigherDeg + expyrs + expyrs2 ~
            PGLoan2016 + home_bachfee + reg_unemp + emp_lag,
          data    = df_local,
          weights = w_local,
          cluster = ~pidp),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  # Extract ALevel return and SE
  coefs <- coef(fit)
  ses   <- se(fit)
  
  tibble(
    w0      = w0,
    ret     = coefs["fit_ALevel"],
    se      = ses["fit_ALevel"],
    n_eff   = sum(w_local)   # effective sample size
  )
})

# ── 3. Compute confidence bands ───────────────────────────────────────────────
local_results <- local_results |>
  mutate(
    ci_low  = ret - 1.96 * se,
    ci_high = ret + 1.96 * se
  )

# ── 4. Plot ───────────────────────────────────────────────────────────────────
ggplot(local_results, aes(x = w0, y = ret)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 1) +
  labs(
    title    = "Local Return to A-Level across the Minimum Wage Distribution",
    subtitle = paste0("Kernel-weighted 2SLS, Epanechnikov kernel, h = ", 
                      round(h, 3)),
    x        = "Real Minimum Wage (£)",
    y        = "Local return coefficient (A-Level)",
    caption  = "Note: 95% pointwise confidence bands. Weighted 2SLS at each MW point."
  ) +
  theme_bw(base_size = 13)