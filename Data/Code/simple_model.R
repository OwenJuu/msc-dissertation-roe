## CONTROL FUNCTIONS MODEL - NO NLW INTERACTION
controls    <- c("imr", "sex", "race", "gor_dv", "NLW")
instruments <- c("PGLoan2016", "Fee2012", "ROSLA2013", "ROSLA2015",
                 "reg_unemp", "emp_lag")

rhs_1st <- paste(
  c(instruments, controls, "gor_dv:factor(year)"),
  collapse = " + "
)

#First Stage regression
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
