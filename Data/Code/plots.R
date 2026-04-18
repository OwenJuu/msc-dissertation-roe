# ============================================================
# Four-panel plot: one chart per qualification / experience
# ============================================================

library(fixest)
library(ggplot2)
library(dplyr)

# ----------------------------------------------------------
# 1. Extract coefficients and VCV
# ----------------------------------------------------------
coefs  <- coef(twfe_iv2)
vcov_m <- vcov(twfe_iv2, cluster = ~pidp)
all_names <- names(coefs)

find_name <- function(keyword, names_vec) {
  hits <- names_vec[grepl(keyword, names_vec, ignore.case = TRUE)]
  if (length(hits) == 0) stop(paste("No match found for keyword:", keyword))
  if (length(hits) >  1) message(paste("Multiple matches for", keyword, "— using:", hits[1]))
  hits[1]
}

nm <- list(
  ALevel    = find_name("^fit_ALevel$|ALevel",       all_names),
  Undergrad = find_name("^fit_Undergrad$|Undergrad",  all_names),
  HigherEd  = find_name("^fit_HigherEd$|HigherEd",   all_names),
  expyrs    = find_name("^fit_expyrs$|^expyrs$",      all_names),
  expyrs2   = find_name("^fit_expyrs2$|^expyrs2$",    all_names),
  
  NLW_ALevel    = find_name("NLW_ALevel",    all_names),
  NLW_Undergrad = find_name("NLW_Undergrad", all_names),
  NLW_HigherEd  = find_name("NLW_HigherEd",  all_names),
  NLW_expyrs    = find_name("NLW_expyrs$",   all_names),
  NLW_expyrs2   = find_name("NLW_expyrs2",   all_names)
)

# ----------------------------------------------------------
# 2. Delta-method helpers
# ----------------------------------------------------------
marginal_return_ci <- function(base_name, int_name, nlw_seq,
                               coefs, vcov_m, alpha = 0.05) {
  b0  <- coefs[base_name]
  b1  <- coefs[int_name]
  V   <- vcov_m[c(base_name, int_name), c(base_name, int_name)]
  
  est    <- b0 + b1 * nlw_seq
  se_est <- sapply(nlw_seq, function(nlw) {
    g <- c(1, nlw)
    sqrt(as.numeric(t(g) %*% V %*% g))
  })
  
  z <- qnorm(1 - alpha / 2)
  data.frame(NLW   = nlw_seq,
             est   = est,
             lower = est - z * se_est,
             upper = est + z * se_est)
}

exp_return_ci <- function(nlw_seq, coefs, vcov_m, nm,
                          mean_exp, alpha = 0.05) {
  nms <- c(nm$expyrs, nm$expyrs2, nm$NLW_expyrs, nm$NLW_expyrs2)
  V   <- vcov_m[nms, nms]
  
  b_e   <- coefs[nm$expyrs];     b_e2  <- coefs[nm$expyrs2]
  b_ne  <- coefs[nm$NLW_expyrs]; b_ne2 <- coefs[nm$NLW_expyrs2]
  
  est    <- (b_e + 2*b_e2*mean_exp) + (b_ne + 2*b_ne2*mean_exp) * nlw_seq
  se_est <- sapply(nlw_seq, function(nlw) {
    g <- c(1, 2*mean_exp, nlw, 2*mean_exp*nlw)
    sqrt(as.numeric(t(g) %*% V %*% g))
  })
  
  z <- qnorm(1 - alpha / 2)
  data.frame(NLW   = nlw_seq,
             est   = est,
             lower = est - z * se_est,
             upper = est + z * se_est)
}

# ----------------------------------------------------------
# 3. Grid + assemble with panel labels
# ----------------------------------------------------------
nlw_seq  <- seq(min(working_df$NLW, na.rm = TRUE),
                max(working_df$NLW, na.rm = TRUE),
                length.out = 200)
mean_exp <- mean(working_df$expyrs, na.rm = TRUE)

exp_label <- paste0("Experience (at mean = ", round(mean_exp, 1), " yrs)")

panel_colours <- c(
  "A-Level"          = "#E69F00",
  "Undergraduate"    = "#56B4E9",
  "Higher Education" = "#009E73"
)
panel_colours[exp_label] <- "#CC79A7"

quals <- list(
  list(label = "A-Level",          base = nm$ALevel,    int = nm$NLW_ALevel),
  list(label = "Undergraduate",    base = nm$Undergrad, int = nm$NLW_Undergrad),
  list(label = "Higher Education", base = nm$HigherEd,  int = nm$NLW_HigherEd)
)

plot_df <- bind_rows(
  lapply(quals, function(q) {
    marginal_return_ci(q$base, q$int, nlw_seq, coefs, vcov_m) |>
      mutate(group = q$label, colour = panel_colours[q$label])
  }),
  exp_return_ci(nlw_seq, coefs, vcov_m, nm, mean_exp) |>
    mutate(group = exp_label, colour = panel_colours[exp_label])
)

# Fix factor order for panel layout
plot_df$group <- factor(plot_df$group,
                        levels = c("A-Level", "Undergraduate",
                                   "Higher Education", exp_label))

# ----------------------------------------------------------
# 4. Four-panel plot via facet_wrap with free y-scales
# ----------------------------------------------------------
ggplot(plot_df, aes(x = NLW, y = est, colour = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ group, nrow = 2, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = panel_colours, guide = "none") +
  scale_fill_manual(  values = panel_colours, guide = "none") +
  scale_x_continuous(
    labels = scales::label_currency(prefix = "£"),
    breaks = pretty(nlw_seq, n = 5)
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1)
  ) +
  labs(
    title    = "Effect of the National Living Wage on Returns to\nQualifications and Experience",
    subtitle = "IV-TWFE estimates; shaded bands = 95% CI (clustered by individual)",
    x        = "National Living Wage level (£/hr)",
    y        = "Return (log-wage coefficient)",
    caption  = paste0(
      "Returns to qualifications are relative to No Qualification baseline.\n",
      "Experience return is marginal effect evaluated at sample mean (",
      round(mean_exp, 1), " years)."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text         = element_text(face = "bold", size = 12),
    strip.background   = element_rect(fill = "grey95", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(1.2, "lines"),
    plot.title         = element_text(face = "bold"),
    plot.caption       = element_text(colour = "grey50", size = 9),
    plot.title.position = "plot"
  )

# Optional save
# ggsave("nlw_returns_panels.png", width = 10, height = 8, dpi = 300)