## CONTROL FUNCTIONS MODEL - NO NLW INTERACTION
controls    <- c("imr", "sex", "race", "gor_dv", "NLW")
instruments <- c("PGLoan2016", "Fee2012", "ROSLA2013", "ROSLA2015",
                 "reg_unemp", "emp_lag")

rhs_1st <- paste(
  c(instruments, controls),
  collapse = " + "
)

#First Stage regression
probit_ALevel    <- glm(
  as.formula(paste("ALevel ~",    rhs_1st)),
  data = working_df, family = binomial(link = "probit")
)
probit_Bachelor  <- glm(
  as.formula(paste("Bachelor ~",  rhs_1st)),
  data = working_df, family = binomial(link = "probit")
)
probit_HigherDeg <- glm(
  as.formula(paste("HigherDeg ~", rhs_1st)),
  data = working_df, family = binomial(link = "probit")
)

ols_expyrs  <- lm(
  as.formula(paste("expyrs ~",  rhs_1st)),
  data = working_df
)
ols_expyrs2 <- lm(
  as.formula(paste("expyrs2 ~", rhs_1st)),
  data = working_df
)

summary(ols_expyrs)

## Function to recover residual from OLS and probit
append_imr <- function(data, model, col_name) {
  
  stopifnot(inherits(model, "glm"), model$family$link == "probit")
  
  idx       <- as.integer(rownames(model.frame(model)))
  xb        <- predict(model, type = "link")
  imr       <- dnorm(xb) / pnorm(xb)
  
  data[[col_name]]      <- NA_real_
  data[[col_name]][idx] <- imr
  
  message(sprintf("'%s' appended. Filled %d / %d rows (%d NAs from dropped obs).",
                  col_name, length(idx), nrow(data), nrow(data) - length(idx)))
  data
}

append_lm_resid <- function(data, model, col_name) {
  
  stopifnot(inherits(model, "lm"))
  
  resids <- residuals(model)          # already named with original row indices
  idx    <- as.integer(names(resids))
  
  data[[col_name]]      <- NA_real_
  data[[col_name]][idx] <- resids
  
  n_dropped <- nrow(data) - length(idx)
  message(sprintf(
    "'%s' appended. Filled %d / %d rows (%d NAs from dropped obs).\nRemember: reassign the result  -->  df <- append_lm_resid(df, model)",
    col_name, length(idx), nrow(data), n_dropped
  ))
  
  data
}

working_df <- append_imr(working_df, probit_ALevel, "v_ALevel")
working_df <- append_imr(working_df, probit_Bachelor, "v_Bachelor")
working_df <- append_imr(working_df, probit_HigherDeg, "v_HigherDeg")
working_df <- append_lm_resid(working_df, ols_expyrs, "v_expyrs")
working_df <- append_lm_resid(working_df, ols_expyrs2, "v_expyrs2")

cfa_model <- feols(
  lwage ~ ALevel + Bachelor + HigherDeg + expyrs + expyrs2  # endogenous
  + imr + sex + race + gor_dv + NLW                         # controls
  + v_ALevel + v_Bachelor + v_HigherDeg                # CF terms (probit)
  + v_expyrs + v_expyrs2,                               # CF terms (OLS)                                              # year FE
  data    = working_df,
  cluster = ~pidp
)

summary(cfa_model)