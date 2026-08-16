# ── NATIONAL KAITZ PLOT ─────────────────────────────────────────────────────
coefs <- coef(model_np)
V     <- vcov(model_np, attr = FALSE)
mw_grid <- seq(min(post2010$Kaitz, na.rm = TRUE),
               max(post2010$Kaitz, na.rm = TRUE),
               length.out = 200)
# Recompute spline basis on the grid using SAME boundary knots as estimation
# FIX: df = 3, not 4 (model_np only has bs(Kaitz, 3)1/2/3)
bk <- attr(bs(post2010$Kaitz, df = 4), "Boundary.knots")
B  <- bs(mw_grid, df = 4, Boundary.knots = bk)  # 200 x 3 matrix

# FIX: model_np DOES have a standalone HigherDeg term (fit_HigherDeg = -25.91),
# so the return is beta_HigherDeg + gamma' * b(w), not just gamma' * b(w)
param_names <- c("fit_HigherDeg",
                 "fit_HigherDeg:bs(Kaitz, 4)1",
                 "fit_HigherDeg:bs(Kaitz, 4)2",
                 "fit_HigherDeg:bs(Kaitz, 4)3",
                 "fit_HigherDeg:bs(Kaitz, 4)4")
theta_A <- coefs[param_names]

# FIX: gradient vector needs a leading 1 to multiply against fit_HigherDeg
C <- cbind(1, B)   # 200 x 4

# Marginal return vector (200 x 1)
returns <- C %*% theta_A

# Return(w) = c(w)' theta, where c(w) = [1, b1(w), b2(w), b3(w)]
# leading 1 restored to pick up the base HigherDeg coefficient
V_sub <- V[param_names, param_names]   # now 4x4, includes base term's variance
se_returns <- sapply(1:nrow(C), function(i) {
  c_w <- C[i, ]                        # 4 x 1 gradient: c(1, B[i,])
  sqrt(as.numeric(t(c_w) %*% V_sub %*% c_w))
})

plot_df <- tibble(
  Kaitz   = mw_grid,
  ret     = as.numeric(returns),
  se      = se_returns,
  ci_low  = ret - 1.96 * se,
  ci_high = ret + 1.96 * se
)

ggplot(subset(plot_df, Kaitz > 0.5), aes(x = Kaitz, y = ret)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 1) +
  #coord_cartesian(ylim = c(-2.5, 5)) + 
  labs(
    x       = "Kaitz index",
    y       = expression("Return coefficient"),
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey40", hjust = 0)
  )


# ── REGIONAL KAITZ PLOT ─────────────────────────────────────────────────────
coefs <- coef(model_npr)
V     <- vcov(model_npr, attr = FALSE)
mw_grid <- seq(min(post2010$rKaitz, na.rm = TRUE),
               max(post2010$rKaitz, na.rm = TRUE),
               length.out = 200)
# Recompute spline basis on the grid using SAME boundary knots as estimation
# FIX: df = 3, not 4 (model_np only has bs(Kaitz, 3)1/2/3)
bk <- attr(bs(post2010$rKaitz, df = 4), "Boundary.knots")
B  <- bs(mw_grid, df = 4, Boundary.knots = bk)  # 200 x 3 matrix

# FIX: model_np DOES have a standalone HigherDeg term (fit_HigherDeg = -25.91),
# so the return is beta_HigherDeg + gamma' * b(w), not just gamma' * b(w)
param_names <- c("fit_HigherDeg",
                 "fit_HigherDeg:bs(rKaitz, 4)1",
                 "fit_HigherDeg:bs(rKaitz, 4)2",
                 "fit_HigherDeg:bs(rKaitz, 4)3",
                 "fit_HigherDeg:bs(rKaitz, 4)4")
theta_A <- coefs[param_names]

# FIX: gradient vector needs a leading 1 to multiply against fit_HigherDeg
C <- cbind(1, B)   # 200 x 4

# Marginal return vector (200 x 1)
returns <- C %*% theta_A

# Return(w) = c(w)' theta, where c(w) = [1, b1(w), b2(w), b3(w)]
# leading 1 restored to pick up the base HigherDeg coefficient
V_sub <- V[param_names, param_names]   # now 4x4, includes base term's variance
se_returns <- sapply(1:nrow(C), function(i) {
  c_w <- C[i, ]                        # 4 x 1 gradient: c(1, B[i,])
  sqrt(as.numeric(t(c_w) %*% V_sub %*% c_w))
})

plot_df <- tibble(
  Kaitz   = mw_grid,
  ret     = as.numeric(returns),
  se      = se_returns,
  ci_low  = ret - 1.96 * se,
  ci_high = ret + 1.96 * se
)

ggplot(subset(plot_df, Kaitz > 0.5), aes(x = Kaitz, y = ret)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 1) +
  #coord_cartesian(ylim = c(-2.5, 5)) + 
  labs(
    x       = "Regional Kaitz index",
    y       = expression("Return coefficient"),
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey40", hjust = 0)
  )


# ── NATIONAL KAITZ PLOT DF = 5 ─────────────────────────────────────────────────────
coefs <- coef(model_np5)
V     <- vcov(model_np5, attr = FALSE)
mw_grid <- seq(min(post2010$Kaitz, na.rm = TRUE),
               max(post2010$Kaitz, na.rm = TRUE),
               length.out = 200)
# Recompute spline basis on the grid using SAME boundary knots as estimation
# FIX: df = 3, not 4 (model_np only has bs(Kaitz, 3)1/2/3)
bk <- attr(bs(post2010$Kaitz, df = 5), "Boundary.knots")
B  <- bs(mw_grid, df = 5, Boundary.knots = bk)  # 200 x 3 matrix

# FIX: model_np DOES have a standalone HigherDeg term (fit_HigherDeg = -25.91),
# so the return is beta_HigherDeg + gamma' * b(w), not just gamma' * b(w)
param_names <- c("fit_HigherDeg",
                 "fit_HigherDeg:bs(Kaitz, 5)1",
                 "fit_HigherDeg:bs(Kaitz, 5)2",
                 "fit_HigherDeg:bs(Kaitz, 5)3",
                 "fit_HigherDeg:bs(Kaitz, 5)4",
                 "fit_HigherDeg:bs(Kaitz, 5)5")
theta_A <- coefs[param_names]

# FIX: gradient vector needs a leading 1 to multiply against fit_HigherDeg
C <- cbind(1, B)   # 200 x 4

# Marginal return vector (200 x 1)
returns <- C %*% theta_A

# Return(w) = c(w)' theta, where c(w) = [1, b1(w), b2(w), b3(w)]
# leading 1 restored to pick up the base HigherDeg coefficient
V_sub <- V[param_names, param_names]   # now 4x4, includes base term's variance
se_returns <- sapply(1:nrow(C), function(i) {
  c_w <- C[i, ]                        # 4 x 1 gradient: c(1, B[i,])
  sqrt(as.numeric(t(c_w) %*% V_sub %*% c_w))
})

plot_df <- tibble(
  Kaitz   = mw_grid,
  ret     = as.numeric(returns),
  se      = se_returns,
  ci_low  = ret - 1.96 * se,
  ci_high = ret + 1.96 * se
)

ggplot(subset(plot_df, Kaitz > 0.5), aes(x = Kaitz, y = ret)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 1) +
  #coord_cartesian(ylim = c(-2.5, 5)) + 
  labs(
    x       = "Kaitz index",
    y       = expression("Return coefficient"),
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey40", hjust = 0)
  )

# ── NATIONAL KAITZ PLOT DF = 3 ─────────────────────────────────────────────────────
coefs <- coef(model_np3)
V     <- vcov(model_np3, attr = FALSE)
mw_grid <- seq(min(post2010$Kaitz, na.rm = TRUE),
               max(post2010$Kaitz, na.rm = TRUE),
               length.out = 200)
# Recompute spline basis on the grid using SAME boundary knots as estimation
# FIX: df = 3, not 4 (model_np only has bs(Kaitz, 3)1/2/3)
bk <- attr(bs(post2010$Kaitz, df = 3), "Boundary.knots")
B  <- bs(mw_grid, df = 3, Boundary.knots = bk)  # 200 x 3 matrix

# FIX: model_np DOES have a standalone HigherDeg term (fit_HigherDeg = -25.91),
# so the return is beta_HigherDeg + gamma' * b(w), not just gamma' * b(w)
param_names <- c("fit_HigherDeg",
                 "fit_HigherDeg:bs(Kaitz, 3)1",
                 "fit_HigherDeg:bs(Kaitz, 3)2",
                 "fit_HigherDeg:bs(Kaitz, 3)3")
theta_A <- coefs[param_names]

# FIX: gradient vector needs a leading 1 to multiply against fit_HigherDeg
C <- cbind(1, B)   # 200 x 4

# Marginal return vector (200 x 1)
returns <- C %*% theta_A

# Return(w) = c(w)' theta, where c(w) = [1, b1(w), b2(w), b3(w)]
# leading 1 restored to pick up the base HigherDeg coefficient
V_sub <- V[param_names, param_names]   # now 4x4, includes base term's variance
se_returns <- sapply(1:nrow(C), function(i) {
  c_w <- C[i, ]                        # 4 x 1 gradient: c(1, B[i,])
  sqrt(as.numeric(t(c_w) %*% V_sub %*% c_w))
})

plot_df <- tibble(
  Kaitz   = mw_grid,
  ret     = as.numeric(returns),
  se      = se_returns,
  ci_low  = ret - 1.96 * se,
  ci_high = ret + 1.96 * se
)

ggplot(subset(plot_df, Kaitz > 0.5), aes(x = Kaitz, y = ret)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 1) +
  #coord_cartesian(ylim = c(-2.5, 5)) + 
  labs(
    x       = "Kaitz index",
    y       = expression("Return coefficient"),
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey40", hjust = 0)
  )

