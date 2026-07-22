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

library(dplyr)
library(tidyr)

# Total number of observations
overall <- tibble(
  Characteristic = "Number of observations",
  All    = nrow(usoc_working_complete),
  Male   = sum(usoc_working_complete$sex == "male", na.rm = TRUE),
  Female = sum(usoc_working_complete$sex == "female", na.rm = TRUE),
  White  = sum(usoc_working_complete$race == "White", na.rm = TRUE),
  Black  = sum(usoc_working_complete$race == "Black", na.rm = TRUE),
  Asian  = sum(usoc_working_complete$race == "Asian", na.rm = TRUE),
  Mixed  = sum(usoc_working_complete$race == "Mixed", na.rm = TRUE)
)

# Region counts
region_table <- usoc_working_complete %>%
  group_by(region) %>%
  summarise(
    All    = n(),
    Male   = sum(sex == "male", na.rm = TRUE),
    Female = sum(sex == "female", na.rm = TRUE),
    White  = sum(race == "White", na.rm = TRUE),
    Black  = sum(race == "Black", na.rm = TRUE),
    Asian  = sum(race == "Asian", na.rm = TRUE),
    Mixed  = sum(race == "Mixed", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Characteristic = region) %>%
  mutate(Characteristic = paste("Region -", Characteristic))

# Combine
summary_table <- bind_rows(overall, region_table)

view(summary_table)
write.csv(
  summary_table,
  file = "summary_table.csv",
  row.names = FALSE
)

education_summary <- data.frame(
  Education = c("lwage", "ALevel", "VOC", "Bachelor", "HigherDeg", "expyrs", "runemp16_devi",
                "runemp18_devi", "runemp22_devi", "rearn16_devi", "rearn18_devi",
                "rearn22_devi", "hbachfee18_real", "runicount16", "PGLoan2016",
                "GCSE", "runemp_current", "rearn_current"),
  
  All = c(
    mean(usoc_working_complete$lwage, na.rm = TRUE),
    mean(usoc_working_complete$ALevel, na.rm = TRUE),
    mean(usoc_working_complete$VOC, na.rm = TRUE),
    mean(usoc_working_complete$Bachelor, na.rm = TRUE),
    mean(usoc_working_complete$HigherDeg, na.rm = TRUE),
    mean(usoc_working_complete$expyrs, na.rm = TRUE),
    mean(usoc_working_complete$runemp16_devi, na.rm = TRUE),
    mean(usoc_working_complete$runemp18_devi, na.rm = TRUE),
    mean(usoc_working_complete$runemp22_devi, na.rm = TRUE),
    mean(usoc_working_complete$rearn16_devi, na.rm = TRUE),
    mean(usoc_working_complete$rearn18_devi, na.rm = TRUE),
    mean(usoc_working_complete$rearn22_devi, na.rm = TRUE),
    mean(usoc_working_complete$hbachfee18_real, na.rm = TRUE),
    mean(usoc_working_complete$runicount16, na.rm = TRUE),
    mean(usoc_working_complete$PGLoan2016, na.rm = TRUE),
    mean(usoc_working_complete$GCSE, na.rm = TRUE),
    mean(usoc_working_complete$runemp_current, na.rm = TRUE),
    mean(usoc_working_complete$rearn_current, na.rm = TRUE)
  ),
  
  male = c(
    mean(usoc_working_complete$lwage[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$ALevel[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$VOC[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$Bachelor[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$HigherDeg[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$expyrs[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$runemp16_devi[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$runemp18_devi[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$runemp22_devi[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$rearn16_devi[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$rearn18_devi[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$rearn22_devi[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$hbachfee18_real[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$runicount16[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$PGLoan2016[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$GCSE[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$runemp_current[usoc_working_complete$sex == "male"], na.rm = TRUE),
    mean(usoc_working_complete$rearn_current[usoc_working_complete$sex == "male"], na.rm = TRUE)
  ),
  
  female = c(
    mean(usoc_working_complete$lwage[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$ALevel[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$VOC[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$Bachelor[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$HigherDeg[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$expyrs[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$runemp16_devi[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$runemp18_devi[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$runemp22_devi[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$rearn16_devi[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$rearn18_devi[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$rearn22_devi[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$hbachfee18_real[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$runicount16[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$PGLoan2016[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$GCSE[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$runemp_current[usoc_working_complete$sex == "female"], na.rm = TRUE),
    mean(usoc_working_complete$rearn_current[usoc_working_complete$sex == "female"], na.rm = TRUE)
  ),
  
  White = c(
    mean(usoc_working_complete$lwage[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$ALevel[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$VOC[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$Bachelor[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$HigherDeg[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$expyrs[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$runemp16_devi[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$runemp18_devi[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$runemp22_devi[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$rearn16_devi[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$rearn18_devi[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$rearn22_devi[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$hbachfee18_real[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$runicount16[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$PGLoan2016[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$GCSE[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$runemp_current[usoc_working_complete$race == "White"], na.rm = TRUE),
    mean(usoc_working_complete$rearn_current[usoc_working_complete$race == "White"], na.rm = TRUE)
  ),
  
  Black = c(
    mean(usoc_working_complete$lwage[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$ALevel[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$VOC[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$Bachelor[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$HigherDeg[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$expyrs[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$runemp16_devi[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$runemp18_devi[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$runemp22_devi[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$rearn16_devi[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$rearn18_devi[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$rearn22_devi[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$hbachfee18_real[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$runicount16[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$PGLoan2016[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$GCSE[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$runemp_current[usoc_working_complete$race == "Black"], na.rm = TRUE),
    mean(usoc_working_complete$rearn_current[usoc_working_complete$race == "Black"], na.rm = TRUE)
  ),
  
  Asian = c(
    mean(usoc_working_complete$lwage[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$ALevel[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$VOC[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$Bachelor[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$HigherDeg[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$expyrs[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$runemp16_devi[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$runemp18_devi[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$runemp22_devi[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$rearn16_devi[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$rearn18_devi[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$rearn22_devi[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$hbachfee18_real[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$runicount16[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$PGLoan2016[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$GCSE[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$runemp_current[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$rearn_current[usoc_working_complete$race == "Asian"], na.rm = TRUE)
  ),
  
  Mixed = c(
    mean(usoc_working_complete$lwage[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$ALevel[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$VOC[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$Bachelor[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$HigherDeg[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$expyrs[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$runemp16_devi[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$runemp18_devi[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$runemp22_devi[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$rearn16_devi[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$rearn18_devi[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$rearn22_devi[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$hbachfee18_real[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$runicount16[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$PGLoan2016[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$GCSE[usoc_working_complete$race == "Mixed"], na.rm = TRUE),
    mean(usoc_working_complete$runemp_current[usoc_working_complete$race == "Asian"], na.rm = TRUE),
    mean(usoc_working_complete$rearn_current[usoc_working_complete$race == "Asian"], na.rm = TRUE)
  )
)

view(education_summary)

write.csv(
  education_summary,
  file = "summary_table.csv",
  row.names = FALSE
)