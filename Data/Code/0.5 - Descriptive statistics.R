##FIGURE 1
rminwage <- read_excel("External Data/mw_cpi.xlsx", sheet = "real_unconverted")
rminwage_long <- rminwage %>%
  pivot_longer(
    cols = -Year,
    names_to = "age",
    values_to = "real_minimum_wage"
  )

ggplot(rminwage_long,
       aes(x = Year,
           y = real_minimum_wage,
           color = age,
           group = age)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "Real Minimum Wage (GBP per week)",
    color = "Age"
  ) +
  theme_minimal()