library(tidyverse)
library(cowplot)

# ── Coefficients ──────────────────────────────────────────────────────────────
coefs <- coef(pooled_NLW_iv, stage = 2)

# Helper to extract cleanly (tries both name formats feols may use)
get_coef <- function(coefs, ...) {
  candidates <- c(...)
  matched <- candidates[candidates %in% names(coefs)]
  if (length(matched) == 0) stop(paste("None of these found in model:", paste(candidates, collapse = ", ")))
  coefs[[matched[1]]]
}

b_ALevel        <- get_coef(coefs, "fit_ALevel",     "ALevel")
b_Bachelor      <- get_coef(coefs, "fit_Bachelor",   "Bachelor")
b_HigherDeg     <- get_coef(coefs, "fit_HigherDeg",  "HigherDeg")
b_expyrs        <- get_coef(coefs, "fit_expyrs",     "expyrs")
b_expyrs2       <- get_coef(coefs, "fit_expyrs2",    "expyrs2")

b_NLW_ALevel    <- get_coef(coefs, "fit_ALevel:NLW",    "ALevel:NLW",    "NLW:ALevel")
b_NLW_Bachelor  <- get_coef(coefs, "fit_Bachelor:NLW",  "Bachelor:NLW",  "NLW:Bachelor")
b_NLW_HigherDeg <- get_coef(coefs, "fit_HigherDeg:NLW", "HigherDeg:NLW", "NLW:HigherDeg")
b_NLW_expyrs    <- get_coef(coefs, "fit_expyrs:NLW",    "expyrs:NLW",    "NLW:expyrs")
b_NLW_expyrs2   <- get_coef(coefs, "fit_expyrs2:NLW",   "expyrs2:NLW",   "NLW:expyrs2")


# ── Variance-Covariance (from your fitted model) ──────────────────────────────
# Extract directly so CIs are based on clustered SEs
vcov_mat <- vcov(pooled_NLW_iv, stage = 2)

# Variances
var_ALevel      <- vcov_mat["fit_ALevel",    "fit_ALevel"]
var_Bachelor    <- vcov_mat["fit_Bachelor",  "fit_Bachelor"]
var_HigherDeg   <- vcov_mat["fit_HigherDeg", "fit_HigherDeg"]

var_NLW_ALevel      <- vcov_mat["NLW:ALevel",    "NLW:ALevel"]
var_NLW_Bachelor    <- vcov_mat["NLW:Bachelor",  "NLW:Bachelor"]
var_NLW_HigherDeg   <- vcov_mat["NLW:HigherDeg", "NLW:HigherDeg"]

# Covariances between main and interaction term (needed for delta method)
cov_ALevel      <- vcov_mat["fit_ALevel",    "NLW:ALevel"]
cov_Bachelor    <- vcov_mat["fit_Bachelor",  "NLW:Bachelor"]
cov_HigherDeg   <- vcov_mat["fit_HigherDeg", "NLW:HigherDeg"]

# ── Delta method SE: SE(β_qual + β_NLW:qual × nlw) ───────────────────────────
# Var = var_main + nlw² × var_inter + 2 × nlw × cov(main, inter)
delta_se <- function(nlw, var_main, var_inter, cov_mi) {
  sqrt(var_main + nlw^2 * var_inter + 2 * nlw * cov_mi)
}

nlw_seq <- seq(min(working_df$NLW, na.rm = TRUE),
               max(working_df$NLW, na.rm = TRUE),
               length.out = 200)
nlw_key    <- c(6.70, 7.83, 8.91, 10.42, 11.44)
nlw_labels <- c("£6.70 (2015)", "£7.83 (2018)", "£8.91 (2021)",
                "£10.42 (2023)", "£11.44 (2024)")
exp_seq     <- seq(0, 30, by = 0.5)
exp_palette <- c("#C6DBEF", "#6BAED6", "#2171B5", "#084594", "#041F4A")

# ── Plot A data: returns + 95% CI bands ───────────────────────────────────────
qual_df <- tibble(NLW = nlw_seq) %>%
  mutate(
    # Point estimates
    est_ALevel    = b_ALevel    + b_NLW_ALevel    * NLW,
    est_Bachelor  = b_Bachelor  + b_NLW_Bachelor  * NLW,
    est_HigherDeg = b_HigherDeg + b_NLW_HigherDeg * NLW,
    
    # Standard errors via delta method
    se_ALevel     = delta_se(NLW, var_ALevel,    var_NLW_ALevel,    cov_ALevel),
    se_Bachelor   = delta_se(NLW, var_Bachelor,  var_NLW_Bachelor,  cov_Bachelor),
    se_HigherDeg  = delta_se(NLW, var_HigherDeg, var_NLW_HigherDeg, cov_HigherDeg),
    
    # 95% CI bounds
    lo_ALevel     = est_ALevel    - 1.96 * se_ALevel,
    hi_ALevel     = est_ALevel    + 1.96 * se_ALevel,
    lo_Bachelor   = est_Bachelor  - 1.96 * se_Bachelor,
    hi_Bachelor   = est_Bachelor  + 1.96 * se_Bachelor,
    lo_HigherDeg  = est_HigherDeg - 1.96 * se_HigherDeg,
    hi_HigherDeg  = est_HigherDeg + 1.96 * se_HigherDeg
  )

# Pivot to long for ggplot
qual_long <- qual_df %>%
  select(NLW, starts_with("est_"), starts_with("lo_"), starts_with("hi_")) %>%
  pivot_longer(
    -NLW,
    names_to      = c(".value", "Qualification"),
    names_pattern = "(est|lo|hi)_(.*)"
  ) %>%
  mutate(Qualification = recode(Qualification,
                                ALevel    = "A-Level",
                                Bachelor  = "Bachelor's Degree",
                                HigherDeg = "Higher Degree"
  ))

breakeven <- qual_long %>%
  group_by(Qualification) %>%
  summarise(
    breakeven_NLW = {
      idx <- which(diff(sign(est)) != 0)
      if (length(idx) > 0) NLW[idx[1]] else NA_real_
    }
  ) %>%
  filter(!is.na(breakeven_NLW))

# ── Plot A ────────────────────────────────────────────────────────────────────
qual_colours <- c(
  "A-Level"           = "#E41A1C",
  "Bachelor's Degree" = "#377EB8",
  "Higher Degree"     = "#4DAF4A"
)

p_qual <- ggplot(qual_long, aes(x = NLW, colour = Qualification, fill = Qualification)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.6) +
  geom_vline(
    data = breakeven,
    aes(xintercept = breakeven_NLW, colour = Qualification),
    linetype = "dotted", linewidth = 0.7, inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
  geom_line(aes(y = est), linewidth = 1.2) +
  scale_colour_manual(values = qual_colours) +
  scale_fill_manual(values = qual_colours) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "£")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "A. Return to Qualifications by NLW Level",
    subtitle = "Marginal effect: β_qual + β_(NLW×qual) × NLW | shading = 95% CI (delta method) | dotted = break-even NLW",
    x        = "National Living Wage (£/hr)",
    y        = "Return on log wage",
    colour   = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ── Plot B: Cumulative experience profile ─────────────────────────────────────
exp_df <- crossing(expyrs = exp_seq, NLW = nlw_key) %>%
  mutate(
    NLW_label  = factor(NLW, levels = nlw_key, labels = nlw_labels),
    lin_coef   = b_expyrs  + b_NLW_expyrs  * NLW,
    quad_coef  = b_expyrs2 + b_NLW_expyrs2 * NLW,
    Return_exp = lin_coef * expyrs + quad_coef * expyrs^2
  )

peak_df <- exp_df %>%
  group_by(NLW_label) %>%
  slice_max(Return_exp, n = 1)

p_exp <- ggplot(exp_df, aes(x = expyrs, y = Return_exp, colour = NLW_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.6) +
  geom_line(linewidth = 1.1) +
  geom_point(data = peak_df, shape = 4, size = 3, stroke = 1.5, show.legend = FALSE) +
  scale_colour_manual(values = setNames(exp_palette, nlw_labels)) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "B. Cumulative Experience–Wage Profile",
    subtitle = "Log-wage return to years of experience | ✕ = peak year",
    x        = "Years of Experience",
    y        = "Return on log wage",
    colour   = "NLW level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ── Plot C: Marginal return to one extra year ─────────────────────────────────
marg_exp_df <- crossing(expyrs = exp_seq, NLW = nlw_key) %>%
  mutate(
    NLW_label   = factor(NLW, levels = nlw_key, labels = nlw_labels),
    lin_coef    = b_expyrs  + b_NLW_expyrs  * NLW,
    quad_coef   = b_expyrs2 + b_NLW_expyrs2 * NLW,
    Marg_return = lin_coef + 2 * quad_coef * expyrs
  )

p_marg <- ggplot(marg_exp_df, aes(x = expyrs, y = Marg_return, colour = NLW_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.6) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = setNames(exp_palette, nlw_labels)) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "C. Marginal Return to One Extra Year of Experience",
    subtitle = "d(log wage)/d(expyrs) evaluated at each experience level",
    x        = "Years of Experience",
    y        = "Marginal return on log wage",
    colour   = "NLW level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ── Combine with cowplot ───────────────────────────────────────────────────────
bottom_row <- plot_grid(p_exp, p_marg, nrow = 1, align = "h", axis = "bt")

combined <- plot_grid(p_qual, bottom_row, nrow = 2, rel_heights = c(1, 1))

title <- ggdraw() +
  draw_label(
    "How a Rising National Living Wage Reshapes Returns to Human Capital",
    fontface = "bold", size = 13, x = 0.5, hjust = 0.5
  )

caption <- ggdraw() +
  draw_label(
    "TSLS-IV estimates with year FE; clustered SEs on pidp. UKHLS.",
    size = 9, x = 0.5, hjust = 0.5, colour = "grey40"
  )

final_plot <- plot_grid(
  title, combined, caption,
  ncol        = 1,
  rel_heights = c(0.05, 1, 0.04)
)

print(final_plot)
# ggsave("nlw_human_capital_returns.pdf", final_plot, width = 13, height = 11)