## CONTROL FUNCTION
sanity <- feols(
  hiqual ~ hiqual + expyrs + expyrs2 +
    imr + sex + race | gor_dv^year,
  
  data = working_df,
  cluster = ~pidp
)

## CONTROL FUNCTIONS MODEL - NO NLW INTERACTION
controls    <- c("imr", "sex", "race", "gor_dv", "NLW")
instruments <- c("PGLoan2016", "Fee2012", "ROSLA2013", "ROSLA2015",
                 "emp_lag", "numChild", "isCare")

rhs_1st <- paste(
  c(instruments, controls, "gor_dv:factor(year)"),
  collapse = " + "
)

instruments <- paste(c("PGLoan2016", "Fee2012", "ROSLA2013", "ROSLA2015",
                       "reg_unemp", "emp_lag"),
                     collapse = " + ")

model1 <- feols(
  lwage ~ hiqual + expyrs + expyrs2 +
    imr + sex + race | gor_dv^year,
  
  data = working_df,
  cluster = ~pidp
)

summary(model1)

model2 <- feols(
  lwage ~ imr + sex + race | gor_dv^year |
    as.numeric(hiqual)  + expyrs + expyrs2 ~ 
    PGLoan2016 + Fee2012 + emp_lag + numChild + isCare,
  
  data = working_df,
  cluster = ~ pidp
)

summary(model2)

#First Stage regression
fs_hiqual <- polr(
  as.formula(paste("hiqual ~", rhs_1st)),
  data = working_df,
  method = "logistic",
  na.action = na.exclude,
  Hess = TRUE
)

summary(fs_hiqual)


fs_ALevel    <- glm(
  as.formula(paste("ALevel ~",    rhs_1st)),
  data = working_df, family = binomial(link = "probit"), na.action = na.exclude
)
fs_Bachelor  <- glm(
  as.formula(paste("Bachelor ~",  rhs_1st)),
  data = working_df, family = binomial(link = "probit"), na.action = na.exclude
)
fs_HigherDeg <- glm(
  as.formula(paste("HigherDeg ~", rhs_1st)),
  data = working_df, family = binomial(link = "probit"), na.action = na.exclude
)
fs_expyrs  <- lm(
  as.formula(paste("expyrs ~",  rhs_1st)),
  data = working_df, na.action = na.exclude
)
fs_expyrs2 <- lm(
  as.formula(paste("expyrs2 ~", rhs_1st)),
  data = working_df, na.action = na.exclude
)

working_df <- working_df %>% mutate(
  v_ALevel    = residuals(fs_ALevel, type = "response"),
  v_Bachelor  = residuals(fs_Bachelor, type = "response"),
  v_HigherDeg = residuals(fs_HigherDeg, type = "response"),
  v_expyrs    = residuals(fs_expyrs),
  v_expyrs2   = residuals(fs_expyrs2),
)


library(fixest)
library(splines)

model_np <- feols(
  lwage ~ 
    ALevel + Bachelor + HigherDeg + expyrs +
    
    # flexible NLW interactions
    ALevel:bs(NLW, df = 4) +
    Bachelor:bs(NLW, df = 4) +
    HigherDeg:bs(NLW, df = 4) +
    bs(expyrs, df = 5):bs(NLW, df = 4) +
    
    imr + sex + race + gor_dv +
    v_ALevel + v_Bachelor + v_HigherDeg +
    v_expyrs + v_ALevel:bs(NLW, df = 4) +
    v_Bachelor:bs(NLW, df = 4) +
    v_HigherDeg:bs(NLW, df = 4) +
    v_expyrs:bs(NLW, df = 4),
  
  data = working_df,
  cluster = ~pidp
)
summary(model_np)


model2 <- feols(
  lwage ~ ALevel + Bachelor + HigherDeg + expyrs + expyrs2  
  + imr + sex + race + v_ALevel + v_Bachelor + v_HigherDeg 
  + v_expyrs + v_expyrs2 | gor_dv^factor(year),                             
  data    = working_df,
  cluster = ~pidp
)

summary(model2)

model3 <- feols(
  lwage ~ ALevel + Bachelor + HigherDeg + expyrs + expyrs2  
  + NLW:ALevel + NLW:Bachelor + NLW:HigherDeg + NLW:expyrs + NLW:expyrs2
  + imr + sex + race + v_ALevel + v_Bachelor + v_HigherDeg + v_expyrs + v_expyrs2
  + NLW:v_ALevel + NLW:v_Bachelor + NLW:v_HigherDeg + NLW:v_expyrs + NLW:v_expyrs2 
  | gor_dv^factor(year),                             
  data    = working_df,
  cluster = ~pidp
)

summary(model3)

## THE REST



library(fixest)
library(splines)
library(dplyr)
library(parallel)

# ── Configuration ─────────────────────────────────────────────────────────────
CFG <- list(
  n_boot   = 200L,
  seed     = 2024L,
  endog    = c("ALevel", "Bachelor", "HigherDeg", "expyrs", "expyrs2"),
  controls = c("imr", "sex", "race"),
  fe       = "gor_dv^factor(year)"
)

# ── Fixed spline knots ────────────────────────────────────────────────────────
SPL <- list(
  knots    = quantile(working_df$NLW, probs = 0.5,    na.rm = TRUE),
  boundary = quantile(working_df$NLW, probs = c(0, 1), na.rm = TRUE),
  cols     = c("spl1", "spl2", "spl3", "spl4")
)

# ── Formula helpers ───────────────────────────────────────────────────────────
spline_interactions <- function(vars, spl_cols = SPL$cols) {
  terms <- unlist(lapply(vars, function(v) paste(v, spl_cols, sep = ":")))
  paste(terms, collapse = " + ")
}

build_ss_formula <- function(endog, controls, fe) {
  resid_vars <- paste0("v_", endog)
  rhs <- paste(
    paste(endog,      collapse = " + "),
    spline_interactions(endog),
    paste(controls,   collapse = " + "),
    paste(resid_vars, collapse = " + "),
    spline_interactions(resid_vars),
    sep = " + \n  "
  )
  as.formula(sprintf("lwage ~ %s | %s", rhs, fe))
}

SS_FORMULA <- build_ss_formula(CFG$endog, CFG$controls, CFG$fe)

# ── Worker helpers ────────────────────────────────────────────────────────────
draw_bootstrap <- function(df, pids) {
  sampled <- sample(pids, length(pids), replace = TRUE)
  bd <- bind_rows(lapply(seq_along(sampled), function(i) {
    d <- df[df$pidp == sampled[i], , drop = FALSE]
    d$.bid <- i
    d
  }))
  bd
}

add_first_stage_residuals <- function(bd, rhs) {
  probit_vars <- c("ALevel", "Bachelor", "HigherDeg")
  ols_vars    <- c("expyrs", "expyrs2")
  
  fit_probit <- function(lhs)
    glm(as.formula(paste(lhs, "~", rhs)), data = bd,
        family = binomial("probit"), na.action = na.exclude)
  
  fit_ols <- function(lhs)
    lm(as.formula(paste(lhs, "~", rhs)), data = bd, na.action = na.exclude)
  
  all_fits <- c(
    setNames(lapply(probit_vars, fit_probit), probit_vars),
    setNames(lapply(ols_vars,    fit_ols),    ols_vars)
  )
  for (v in names(all_fits))
    bd[[paste0("v_", v)]] <- residuals(all_fits[[v]], type = "response")
  bd
}

add_spline_cols <- function(bd, spl = SPL) {
  mat           <- bs(bd$NLW, knots = spl$knots, Boundary.knots = spl$boundary)
  colnames(mat) <- spl$cols
  cbind(bd, mat)
}

# ── Main model (full data, for point estimates) ───────────────────────────────
main_df <- working_df |>
  add_first_stage_residuals(rhs_1st) |>
  add_spline_cols()

main_model <- feols(SS_FORMULA, data = main_df, cluster = ~pidp)

# ── Single bootstrap iteration ────────────────────────────────────────────────
one_boot <- function(b) {
  tryCatch({
    bd <- draw_bootstrap(working_df, pids)
    bd <- add_first_stage_residuals(bd, rhs_1st)
    bd <- add_spline_cols(bd)
    m  <- feols(SS_FORMULA, data = bd, cluster = ~.bid)
    coef(m)
  }, error = function(e) {
    message(sprintf("Boot %d failed: %s", b, conditionMessage(e)))
    NULL
  })
}

# ── Parallel execution ────────────────────────────────────────────────────────
pids    <- unique(working_df$pidp)
n_cores <- max(1L, detectCores() - 3L)
cl      <- makeCluster(n_cores)

clusterExport(cl, varlist = c(
  "working_df", "pids", "rhs_1st",
  "SPL", "SS_FORMULA",
  "draw_bootstrap", "add_first_stage_residuals", "add_spline_cols",
  "spline_interactions"
))
clusterEvalQ(cl, {
  library(fixest); library(splines); library(dplyr)
  setFixest_nthreads(1)
  options(fixest.notes = FALSE)
})
clusterSetRNGStream(cl, iseed = CFG$seed)

results <- parLapply(cl, seq_len(CFG$n_boot), one_boot)
stopCluster(cl)

# ── Collect bootstrap SEs ─────────────────────────────────────────────────────
results_ok <- Filter(Negate(is.null), results)
n_failed   <- CFG$n_boot - length(results_ok)
if (n_failed > 0)
  message(sprintf("%d/%d iterations failed and were dropped.", n_failed, CFG$n_boot))

ref_names  <- names(results_ok[[1]])
boot_coefs <- do.call(rbind, lapply(results_ok, function(x) {
  out        <- setNames(rep(NA_real_, length(ref_names)), ref_names)
  out[intersect(names(x), ref_names)] <- x[intersect(names(x), ref_names)]
  out
}))

boot_vcov <- cov(boot_coefs, use = "complete.obs")
boot_se   <- sqrt(diag(boot_vcov))

# ── Final results table ───────────────────────────────────────────────────────
main_coefs <- coef(main_model)

# Align: only keep coefficients present in both the main model and bootstrap
shared     <- intersect(names(main_coefs), names(boot_se))
coef_vec   <- main_coefs[shared]
se_vec     <- boot_se[shared]
t_vec      <- coef_vec / se_vec
p_vec      <- 2 * pnorm(-abs(t_vec))

results_table <- data.frame(
  Coefficient = coef_vec,
  Boot_SE     = se_vec,
  t_stat      = t_vec,
  p_value     = p_vec,
  Significance = case_when(
    p_vec < 0.01 ~ "***",
    p_vec < 0.05 ~ "**",
    p_vec < 0.10 ~ "*",
    TRUE         ~ ""
  )
)

print(results_table, digits = 4)