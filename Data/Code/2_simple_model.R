controls    <- c("imr", "sex", "race", "gor_dv", "NLW")
instruments <- c("PGLoan2016", "Fee2012", "reg_unemp", "reg_hep", "emp_lag")
my_vars <- c("lwage", "year", "hiqual", "ALevel", "OtherDip", "Bachelor", "HigherDeg", 
             "expyrs", "expyrs2", "PGLoan2016", "Fee2012", "reg_unemp", 
             "reg_hep", "emp_lag", "numChild", "imr", "sex", "race", "gor_dv")

# SIMPLE LINEAR
simple_linear <- feols(
  lwage ~ ALevel + OtherDip + Bachelor + HigherDeg + expyrs + expyrs2  
  + imr + sex + race | gor_dv^factor(year),                             
  data    = working_df,
  cluster = ~pidp
)

summary(simple_linear)

# IVQR
working_df$gor_dv = droplevels(working_df$gor_dv)
qual_map <- c("NoQual" = 0, "GCSE" = 1, "ALevel" = 2, 
              "OtherDip" = 2.5, "Bachelor" = 3, "HigherDeg" = 4)
working_df <- na.omit(working_df[, my_vars])
working_df <- working_df %>% mutate(
  hiqual_num = qual_map[as.character(working_df$hiqual)]
)

taus <- seq(0.1,0.9,0.01)
grid <-seq(0,25000,100)

ivqr_model <- lwage ~ hiqual_num + expyrs |
  PGLoan2016 + Fee2012 + reg_unemp + reg_hep + emp_lag + numChild | imr + sex + race +
  gor_dv

ivqr <- ivqr(formula = ivqr_model, taus =taus, 
             data =working_df, grid =grid, qrMethod = 'br')

summary(simple_linear)