library(fixest)
library(splines)
library(dplyr)
library(parallel)

# ── Fixed knots (unchanged) ───────────────────────────────────────────────────
nlw_knot     <- quantile(working_df$NLW, probs = 0.5,    na.rm = TRUE)
nlw_boundary <- quantile(working_df$NLW, probs = c(0,1), na.rm = TRUE)

set.seed(2024)
N_BOOT  <- 1
pids    <- unique(working_df$pidp)
n_clust <- length(pids)

# ── Self-contained single-iteration function ──────────────────────────────────
# Everything the worker needs is either passed in or captured from the parent
# environment via clusterExport below.
one_boot <- function(b) {
  
  sampled_pids <- sample(pids, n_clust, replace = TRUE)
  boot_list <- lapply(seq_along(sampled_pids), function(i) {
    d <- working_df[working_df$pidp == sampled_pids[i], , drop = FALSE]
    d$.bid <- i
    d
  })
  bd <- bind_rows(boot_list)
  
  tryCatch({
    
    fs_A  <- glm(as.formula(paste("ALevel ~",    rhs_1st)), data = bd,
                 family = binomial("probit"), na.action = na.exclude)
    fs_B  <- glm(as.formula(paste("Bachelor ~",  rhs_1st)), data = bd,
                 family = binomial("probit"), na.action = na.exclude)
    fs_H  <- glm(as.formula(paste("HigherDeg ~", rhs_1st)), data = bd,
                 family = binomial("probit"), na.action = na.exclude)
    fs_e  <- lm(as.formula(paste("expyrs ~",     rhs_1st)), data = bd,
                na.action = na.exclude)
    fs_e2 <- lm(as.formula(paste("expyrs2 ~",    rhs_1st)), data = bd,
                na.action = na.exclude)
    
    bd <- bd %>% mutate(
      v_ALevel    = residuals(fs_A,  type = "response"),
      v_Bachelor  = residuals(fs_B,  type = "response"),
      v_HigherDeg = residuals(fs_H,  type = "response"),
      v_expyrs    = residuals(fs_e),
      v_expyrs2   = residuals(fs_e2)
    )
    
    # ── Pre-compute spline columns with fixed knots ───────────────────────────
    spl_mat         <- bs(bd$NLW, knots = nlw_knot, Boundary.knots = nlw_boundary)
    colnames(spl_mat) <- c("spl1", "spl2", "spl3", "spl4")
    bd              <- cbind(bd, spl_mat)
    
    m <- feols(
      lwage ~
        ALevel + Bachelor + HigherDeg + expyrs + expyrs2 +
        ALevel:spl1 + ALevel:spl2 + ALevel:spl3 + ALevel:spl4 +
        Bachelor:spl1 + Bachelor:spl2 + Bachelor:spl3 + Bachelor:spl4 +
        HigherDeg:spl1 + HigherDeg:spl2 + HigherDeg:spl3 + HigherDeg:spl4 +
        expyrs:spl1 + expyrs:spl2 + expyrs:spl3 + expyrs:spl4 +
        expyrs2:spl1 + expyrs2:spl2 + expyrs2:spl3 + expyrs2:spl4 +
        imr + sex + race +
        v_ALevel + v_Bachelor + v_HigherDeg + v_expyrs + v_expyrs2 +
        v_ALevel:spl1 + v_ALevel:spl2 + v_ALevel:spl3 + v_ALevel:spl4 +
        v_Bachelor:spl1 + v_Bachelor:spl2 + v_Bachelor:spl3 + v_Bachelor:spl4 +
        v_HigherDeg:spl1 + v_HigherDeg:spl2 + v_HigherDeg:spl3 + v_HigherDeg:spl4 +
        v_expyrs:spl1 + v_expyrs:spl2 + v_expyrs:spl3 + v_expyrs:spl4 +
        v_expyrs2:spl1 + v_expyrs2:spl2 + v_expyrs2:spl3 + v_expyrs2:spl4 |
        gor_dv^factor(year),
      data    = bd,
      cluster = ~.bid
    )
    coef(m)
    
  }, error = function(e) {
    message(sprintf("Boot %d failed: %s", b, conditionMessage(e)))
    NULL
  })
}

# ── Spin up cluster ───────────────────────────────────────────────────────────
n_cores <- max(1L, detectCores() - 1L)
cl      <- makeCluster(n_cores)

# Ship data + settings each worker needs
clusterExport(cl, varlist = c(
  "working_df", "pids", "n_clust", "rhs_1st",
  "nlw_knot", "nlw_boundary"
))

# Load packages and configure fixest on every worker
clusterEvalQ(cl, {
  library(fixest)
  library(splines)
  library(dplyr)
  setFixest_nthreads(1)          # prevent nested CPU fights
  options(fixest.notes = FALSE)  # silence the NA-removal notes
})

# Reproducible parallel RNG (L'Ecuyer-CMRG — safe across workers)
clusterSetRNGStream(cl, iseed = 2024)

# ── Run ───────────────────────────────────────────────────────────────────────
results <- parLapply(cl, seq_len(N_BOOT), one_boot)
stopCluster(cl)

# ── Collect & align ───────────────────────────────────────────────────────────
results_ok <- Filter(Negate(is.null), results)
n_failed   <- N_BOOT - length(results_ok)
if (n_failed > 0)
  message(sprintf("%d/%d iterations failed and were dropped.", n_failed, N_BOOT))

# Name-matched rbind guards against rare collinearity drops
ref_names  <- names(results_ok[[1]])
boot_coefs <- do.call(rbind, lapply(results_ok, function(x) {
  out          <- setNames(rep(NA_real_, length(ref_names)), ref_names)
  shared       <- intersect(names(x), ref_names)
  out[shared]  <- x[shared]
  out
}))

boot_vcov <- cov(boot_coefs, use = "complete.obs")
boot_se   <- sqrt(diag(boot_vcov))