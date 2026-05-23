library(splines)
library(ggplot2)

# NLW grid
nlw_grid <- seq(
  min(working_df$NLW, na.rm = TRUE),
  max(working_df$NLW, na.rm = TRUE),
  length.out = 200
)

# spline basis using SAME specification as model
B <- bs(nlw_grid, df = 4)

# coefficients
b0 <- coef(model_np)["HigherDeg"]

b_spline <- coef(model_np)[
  grep("HigherDeg:bs\\(NLW, df = 4\\)", names(coef(model_np)))
]

# estimated return function
return_HigherDeg <- b0 + as.matrix(B) %*% b_spline

plot_df <- data.frame(
  NLW = nlw_grid,
  Return = as.numeric(return_HigherDeg)
)

ggplot(plot_df, aes(NLW, Return)) +
  geom_line() +
  labs(
    y = "Return to HigherDeg",
    title = "Return to HigherDeg across NLW"
  )