## FIGURE 1
rminwage <- read_excel("External Data/mw_cpi.xlsx", sheet = "real_unconverted")

# Separate Kaitz from wage series
kaitz_df <- rminwage %>% dplyr::select(Year, Kaitz)

wage_cols <- setdiff(names(rminwage), c("Year", "Kaitz"))

rminwage_long <- rminwage %>%
  dplyr::select(Year, all_of(wage_cols)) %>%
  pivot_longer(
    cols = -Year,
    names_to  = "age",
    values_to = "real_minimum_wage"
  )

# Scale factor to map Kaitz (0–1) onto the wage axis
kaitz_scale <- max(rminwage_long$real_minimum_wage, na.rm = TRUE) /
  max(kaitz_df$Kaitz, na.rm = TRUE)

ggplot() +
  # Kaitz columns on the primary axis (scaled)
  geom_col(
    data = kaitz_df,
    aes(x = Year, y = Kaitz * kaitz_scale),
    fill  = "grey80",
    alpha = 0.6,
    width = 0.6
  ) +
  # Wage lines
  geom_line(
    data = rminwage_long,
    aes(x = Year, y = real_minimum_wage, color = age, group = age),
    size = 1,
    na.rm = TRUE
  ) +
  geom_point(
    data = rminwage_long,
    aes(x = Year, y = real_minimum_wage, color = age, group = age),
    size = 1.5,
    na.rm = TRUE
  ) +
  # Dual axis: right axis rescales back to Kaitz units
  scale_y_continuous(
    name = "Real Hourly Minimum Wage (£)",
    sec.axis = sec_axis(
      transform = ~ . / kaitz_scale,
      name      = "Kaitz Index"
    )
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x     = "Year",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left  = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right  = element_text(color = "black"),
    legend.position    = "right"
  ) +
  guides(color = guide_legend(nrow = 9))