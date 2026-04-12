# --- Keep log-point interpretation (DO NOT multiply by 100)

ggplot(bands, aes(x = NLW, y = est, colour = Qualification)) +
  
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.05, linewidth = 0.5,
                position = position_dodge(width = 0.1)) +
  
  # Points
  geom_point(size = 2,
             position = position_dodge(width = 0.1)) +
  
  # Reference: zero return
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.4) +
  
  # Reference: experience return
  geom_hline(yintercept = exp_ret,
             linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  
  # Subtle NLW policy lines (no labels = cleaner)
  geom_vline(data = nlw_breaks, aes(xintercept = xint),
             linetype = "dotted", colour = "grey75", linewidth = 0.3) +
  
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  
  scale_x_continuous(
    name = "National Living Wage (£ per hour)",
    breaks = seq(6, 11, 1),
    labels = function(x) paste0("£", x)
  ) +
  
  scale_y_continuous(
    name = "Return to qualification (log wage points)",
    labels = function(x) sprintf("%.3f", x)
  ) +
  
  labs(
    title = "Returns to education across the National Living Wage",
    subtitle = "Marginal effects from IV fixed-effects model",
    caption = paste0(
      "Notes: Returns computed as β_q + β_(NLW×q) × NLW. ",
      "Shaded areas are 95% confidence intervals (clustered by individual). ",
      "Dashed line shows return to an additional year of experience."
    ),
    colour = NULL,
    fill   = NULL
  ) +
  
  theme_classic(base_size = 11, base_family = "serif") +
  theme(
    plot.title    = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, colour = "grey30", margin = margin(b = 8)),
    plot.caption  = element_text(size = 7.5, colour = "grey45", hjust = 0),
    
    axis.title = element_text(size = 10),
    axis.text  = element_text(size = 9, colour = "grey30"),
    
    # Cleaner legend
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.text = element_text(size = 9),
    
    panel.grid  = element_blank(),
    axis.line   = element_line(colour = "grey70", linewidth = 0.4),
    
    plot.margin = margin(10, 15, 8, 10)
  )