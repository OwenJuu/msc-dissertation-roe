library(tidyverse)
library(splines)

# ── 1. Extract coefficients and vcov ─────────────────────────────────────────
coefs <- coef(model_np)
V     <- vcov(model_np, attr = FALSE)

# ── 2. Define Kaitz grid ─────────────────────────────────────────────────────
mw_grid <- seq(min(usoc_working$Kaitz, na.rm = TRUE),
               max(usoc_working$Kaitz, na.rm = TRUE),
               length.out = 200)

# Recompute spline basis on the grid using SAME boundary knots as estimation
bk  <- attr(bs(usoc_working$Kaitz, df = 4), "Boundary.knots")
B   <- bs(mw_grid, df = 4, Boundary.knots = bk)  # 200 x 3 matrix

# ── 3. Compute marginal return to HigherDeg at each MW point ────────────────────
# Return = beta_HigherDeg + gamma1*b1(w) + gamma2*b2(w) + gamma3*b3(w)

beta_A  <- coefs["fit_HigherDeg"]
gamma_A <- coefs[c("fit_HigherDeg:bs(Kaitz, 4)1",
                   "fit_HigherDeg:bs(Kaitz, 4)2",
                   "fit_HigherDeg:bs(Kaitz, 4)3",
                   "fit_HigherDeg:bs(Kaitz, 4)4")]

# Marginal return vector (200 x 1)
# Return(w) = beta_A + B %*% gamma_A
returns <- beta_A + B %*% gamma_A

# ── 4. Compute standard errors via delta method ───────────────────────────────
# Return(w) = c(w)' theta, where c(w) = [0,...,1(HigherDeg),...,b1,b2,b3,...]
# We need to extract the relevant rows/cols from vcov

param_names <- c("fit_HigherDeg",
                 "fit_HigherDeg:bs(Kaitz, 4)1",
                 "fit_HigherDeg:bs(Kaitz, 4)2",
                 "fit_HigherDeg:bs(Kaitz, 4)3",
                 "fit_HigherDeg:bs(Kaitz, 4)4")

V_sub <- V[param_names, param_names]

# For each MW point, gradient vector c(w) = [1, b1(w), b2(w), b3(w)]
se_returns <- sapply(1:nrow(B), function(i) {
  c_w <- c(1, B[i, ])                  # 4 x 1 gradient
  sqrt(as.numeric(t(c_w) %*% V_sub %*% c_w))
})

# ── 5. Assemble plot dataframe ────────────────────────────────────────────────
plot_df <- tibble(
  Kaitz  = mw_grid,
  ret     = as.numeric(returns),
  se      = se_returns,
  ci_low  = ret - 1.96 * se,
  ci_high = ret + 1.96 * se
)

# ── 6. Plot ───────────────────────────────────────────────────────────────────
ggplot(subset(plot_df, Kaitz > 0.5), aes(x = Kaitz, y = ret)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 1) +
  labs(
    x        = "Kaitz index",
    y        = expression("Return coefficient of HigherDeg  " * (hat(beta)[A](w))),
    caption  = "Note: Shaded area shows 95% pointwise confidence band.\nEstimated via delta method on clustered standard errors."
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey40", hjust = 0)
  )


