# Dummy Kalman Filter
# Model: log(P_dep) = sum_k[alpha_k*R_k(t)] + [beta_base_t + sum_b omega_b*S_b(t)]*log(P_ind) + delta_t*log(Brent_MA3) + AR(2) + eps
# States: regime alphas (fixed), beta_base_t, omega_b, delta_t (random walks), phi1, phi2 (fixed)
# R_k: regime dummies; S_b: slope-shift dummies from spread breaks
# Specification tests regime-shift robustness against gradual time-varying integration
# Inputs: gas_clean_balanced.csv, spread break dates

pkgs <- c("dplyr", "readr", "tidyr", "tibble", "dlm", "moments",
          "tseries", "here")
to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

project_dir <- here::here()
input_file  <- file.path(project_dir, "Model", "model output",
                         "Data Cleaning", "gas_clean_balanced.csv")
breaks_file <- file.path(project_dir, "Model", "model output",
                         "Unit Root and Structural Break",
                         "Pairwise Spreads", "spread_BaiPerron_breaks.csv")
kf_dir      <- file.path(project_dir, "Model", "model output", 
                         "Kalman filter", "Dummy Kalman Filter")
dir.create(kf_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load ---
if (!file.exists(input_file)) stop("Input file not found: ", input_file)
if (!file.exists(breaks_file)) stop("Spread break dates file not found: ", breaks_file)

gas <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

# --- Input validation ---
hubs <- c("JKM", "NBP", "TTF", "HH")
required_cols <- c("Date", hubs, "Brent_crude", "Brent_MA3")
missing_cols <- setdiff(required_cols, names(gas))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
       "\n  Use gas_clean_balanced.csv with Brent_MA3.")
}
if (anyNA(gas[hubs])) stop("Input contains missing gas prices.")
if (any(gas[hubs] <= 0, na.rm = TRUE)) stop("Non-positive gas prices found.")
if (all(is.na(gas$Brent_MA3))) stop("Brent_MA3 is entirely missing. Re-run Data Cleaning.")
if (any(gas$Brent_MA3 <= 0, na.rm = TRUE)) stop("Non-positive Brent_MA3 values found.")

breaks_raw <- read_csv(breaks_file, show_col_types = FALSE) |>
  mutate(Break_date = as.Date(Break_date))

# Defensive gas-gas filtering
if ("Type" %in% names(breaks_raw)) {
  breaks_raw <- breaks_raw |> filter(Type == "Gas-Gas")
}

# Build regime and slope-shift dummies from spread breaks
build_dummies <- function(dates, dep_hub, ind_hub, breaks_df) {
  pair_breaks <- breaks_df |>
    filter((Hub_A == dep_hub & Hub_B == ind_hub) |
             (Hub_A == ind_hub & Hub_B == dep_hub)) |>
    arrange(Break_date) |>
    pull(Break_date) |>
    unique()

  # Keep only valid break dates strictly inside the model sample.
  # This avoids empty regimes when Brent_MA3 burn-in shortens the sample.
  pair_breaks <- pair_breaks[
    !is.na(pair_breaks) &
      pair_breaks > min(dates) &
      pair_breaks < max(dates)
  ]

  n <- length(dates)
  B <- length(pair_breaks)
  K <- B + 1

  regime <- rep(1L, n)
  for (b in seq_along(pair_breaks)) {
    regime[dates >= pair_breaks[b]] <- b + 1L
  }

  R_mat <- matrix(0, nrow = n, ncol = K)
  for (k in seq_len(K)) {
    R_mat[, k] <- as.numeric(regime == k)
  }
  colnames(R_mat) <- paste0("Regime_", seq_len(K))

  S_mat <- NULL
  if (B > 0) {
    S_mat <- matrix(0, nrow = n, ncol = B)
    for (b in seq_len(B)) {
      S_mat[, b] <- as.numeric(dates >= pair_breaks[b])
    }
    colnames(S_mat) <- paste0("Step_", seq_len(B))
  }

  list(
    R_mat = R_mat,
    S_mat = S_mat,
    regime = regime,
    K = K,
    B = B,
    break_dates = pair_breaks
  )
}

# Hub pairs (12 directed equations)
hub_pairs <- list()
for (i in seq_along(hubs)) {
  for (j in seq_along(hubs)) {
    if (i != j) hub_pairs[[length(hub_pairs) + 1]] <- c(hubs[i], hubs[j])
  }
}

# AR(2) stability check
check_ar2_stability <- function(phi1, phi2) {
  denom <- 1 - phi1 - phi2
  roots <- polyroot(c(1, -phi1, -phi2))
  min_modulus <- min(Mod(roots))

  list(
    ar_denom = denom,
    min_root_modulus = min_modulus,
    stable = min_modulus > 1,
    denom_caution = abs(denom) < 0.05
  )
}

# Kalman filter with regime alphas, slope-shift dummies, and oil
V_FLOOR <- 1e-6

run_kalman_regime_dummies_oil <- function(y, x, o, y_lag1, y_lag2, R_mat, S_mat) {
  n <- length(y)
  K <- ncol(R_mat)
  B <- if (!is.null(S_mat)) ncol(S_mat) else 0L

  # States: K alphas + beta_base + B omegas + delta + phi1 + phi2
  p <- K + 1 + B + 3

  alpha_idx <- 1:K
  beta_idx  <- K + 1
  omega_idx <- if (B > 0) (K + 2):(K + 1 + B) else integer(0)
  delta_idx <- K + 1 + B + 1
  phi1_idx  <- K + 1 + B + 2
  phi2_idx  <- K + 1 + B + 3

  # --- Design matrix ---
  if (B > 0) {
    XS_mat <- S_mat * as.numeric(x)
    X_mat <- cbind(R_mat, x = x, XS_mat, o = o, y_lag1 = y_lag1, y_lag2 = y_lag2)
  } else {
    X_mat <- cbind(R_mat, x = x, o = o, y_lag1 = y_lag1, y_lag2 = y_lag2)
  }

  # --- OLS initialisation ---
  ols_df <- as.data.frame(X_mat)
  ols_df$y <- y

  ols_formula <- as.formula(
    paste0("y ~ 0 + ", paste(colnames(X_mat), collapse = " + "))
  )

  fit_ols <- lm(ols_formula, data = ols_df)
  init_coefs <- coef(fit_ols)
  init_coefs[is.na(init_coefs)] <- 0

  # Initial slope-shift effects are set to zero.
  if (length(omega_idx) > 0) {
    init_coefs[omega_idx] <- 0
  }

  build <- function(par) {
    W_diag <- rep(0, p)
    W_diag[beta_idx]  <- exp(par[2])
    W_diag[delta_idx] <- exp(par[3])

    C0_diag <- rep(100, p)
    if (length(omega_idx) > 0) {
      C0_diag[omega_idx] <- 1.0
    }

    dlm(
      m0  = unname(init_coefs),
      C0  = diag(C0_diag),
      FF  = matrix(0, 1, p),
      V   = matrix(exp(par[1]) + V_FLOOR, 1, 1),
      GG  = diag(p),
      W   = diag(W_diag),
      JFF = matrix(1:p, 1, p),
      X   = X_mat
    )
  }

  # --- Two-stage MLE ---
  obs_var_init <- log(var(y, na.rm = TRUE))

  p1_grid <- obs_var_init + c(-3, -2, -1, 0, 1, 2)
  p2_grid <- c(-3, -5, -7, -9, -11, -13, -15)
  p3_grid <- c(-3, -5, -7, -9, -11, -13, -15)

  start_grid <- expand.grid(
    p1 = p1_grid,
    p2 = p2_grid,
    p3 = p3_grid
  )

  coarse_results <- list()

  for (r in seq_len(nrow(start_grid))) {
    fit <- try(
      dlmMLE(
        y,
        parm = as.numeric(start_grid[r, ]),
        build = build,
        method = "Nelder-Mead",
        control = list(maxit = 50)
      ),
      silent = TRUE
    )

    if (!inherits(fit, "try-error")) {
      coarse_results[[length(coarse_results) + 1]] <- fit
    }
  }

  if (length(coarse_results) == 0) {
    warning("MLE did not converge — using fallback.")
    best_fit <- list(par = c(obs_var_init, -8, -8), value = NA, convergence = 1)
    best_nll <- NA_real_
  } else {
    nll_vals <- sapply(coarse_results, function(f) f$value)
    finite_idx <- which(is.finite(nll_vals))

    if (length(finite_idx) == 0) {
      warning("All coarse MLE values non-finite — using fallback.")
      best_fit <- list(par = c(obs_var_init, -8, -8), value = NA, convergence = 1)
      best_nll <- NA_real_
    } else {
      top_idx <- finite_idx[
        order(nll_vals[finite_idx])[1:min(5, length(finite_idx))]
      ]

      best_fit <- NULL
      best_nll <- Inf
      best_fit_nc <- NULL
      best_nll_nc <- Inf

      for (idx in top_idx) {
        fit <- try(
          dlmMLE(
            y,
            parm = coarse_results[[idx]]$par,
            build = build,
            method = "Nelder-Mead",
            control = list(maxit = 500)
          ),
          silent = TRUE
        )

        if (!inherits(fit, "try-error") && is.finite(fit$value)) {
          if (fit$convergence == 0 && fit$value < best_nll) {
            best_nll <- fit$value
            best_fit <- fit
          } else if (fit$value < best_nll_nc) {
            best_nll_nc <- fit$value
            best_fit_nc <- fit
          }
        }
      }

      if (is.null(best_fit)) {
        if (!is.null(best_fit_nc)) {
          best_fit <- best_fit_nc
          best_nll <- best_nll_nc
        } else {
          best_fit <- list(par = c(obs_var_init, -8, -8), value = NA, convergence = 1)
          best_nll <- NA_real_
        }
      }
    }
  }

  mle_converged <- !is.null(best_fit$convergence) && best_fit$convergence == 0

  # --- Filter and smooth ---
  mod <- build(best_fit$par)
  filt <- dlmFilter(y, mod)
  sm <- dlmSmooth(filt)

  S_sm <- sm$s[-1, , drop = FALSE]
  vl_sm <- dlmSvd2var(sm$U.S, sm$D.S)[-1]

  # --- Extract states ---
  alpha_mat <- S_sm[, alpha_idx, drop = FALSE]
  alpha_eff <- rowSums(alpha_mat * R_mat)

  alpha_var_mat <- matrix(
    sapply(seq_len(n), function(t) {
      sapply(alpha_idx, function(k) vl_sm[[t]][k, k])
    }),
    nrow = K
  ) |> t()

  sd_alpha_eff <- sqrt(rowSums(alpha_var_mat * R_mat))

  beta_base <- S_sm[, beta_idx]
  sd_beta_base <- sqrt(sapply(vl_sm, function(m) m[beta_idx, beta_idx]))

  delta_t <- S_sm[, delta_idx]
  sd_delta <- sqrt(sapply(vl_sm, function(m) m[delta_idx, delta_idx]))

  # --- Omega and effective beta ---
  omega_vals <- NULL

  if (B > 0) {
    omega_mat <- S_sm[, omega_idx, drop = FALSE]
    omega_vals <- as.numeric(omega_mat[n, ])

    omega_contribution <- rowSums(omega_mat * S_mat)
    beta_eff <- beta_base + omega_contribution

    var_beta_eff <- sapply(seq_len(n), function(t) {
      vb <- vl_sm[[t]][beta_idx, beta_idx]
      vo <- sum(sapply(omega_idx, function(j) vl_sm[[t]][j, j]) * S_mat[t, ]^2)
      cv <- 2 * sum(sapply(omega_idx, function(j) vl_sm[[t]][beta_idx, j]) * S_mat[t, ])
      max(vb + vo + cv, 0)
    })

    sd_beta_eff <- sqrt(var_beta_eff)
  } else {
    beta_eff <- beta_base
    sd_beta_eff <- sd_beta_base
  }

  phi1_t <- S_sm[, phi1_idx]
  phi2_t <- S_sm[, phi2_idx]
  sd_phi1 <- sqrt(sapply(vl_sm, function(m) m[phi1_idx, phi1_idx]))
  sd_phi2 <- sqrt(sapply(vl_sm, function(m) m[phi2_idx, phi2_idx]))

  # --- Long-run beta_eff and delta ---
  ar_denom_t <- 1 - phi1_t - phi2_t

  beta_eff_lr <- ifelse(
    abs(ar_denom_t) > 1e-4,
    beta_eff / ar_denom_t,
    NA_real_
  )

  sd_beta_eff_lr <- ifelse(
    abs(ar_denom_t) > 1e-4,
    sd_beta_eff / abs(ar_denom_t),
    NA_real_
  )

  delta_lr_t <- ifelse(
    abs(ar_denom_t) > 1e-4,
    delta_t / ar_denom_t,
    NA_real_
  )

  sd_delta_lr <- ifelse(
    abs(ar_denom_t) > 1e-4,
    sd_delta / abs(ar_denom_t),
    NA_real_
  )

  # --- Gas-oil regressor collinearity ---
  cor_x_o <- cor(x, o, use = "complete.obs")

  # --- Standardised residuals ---
  P_filt <- dlmSvd2var(filt$U.C, filt$D.C)
  V_obs <- mod$V[1, 1]
  W_mat_mod <- mod$W

  raw_res <- as.numeric(y) - as.numeric(filt$f)
  F_vec <- numeric(n)

  for (t in seq_len(n)) {
    P_pred <- P_filt[[t]] + W_mat_mod
    ff_t <- X_mat[t, ]
    F_vec[t] <- as.numeric(crossprod(ff_t, P_pred %*% ff_t)) + V_obs
  }

  if (any(F_vec < V_obs)) {
    warning("F_t < V_obs: ", sum(F_vec < V_obs), " of ", n)
  }

  if (any(F_vec <= 0)) {
    warning("F_t <= 0: ", sum(F_vec <= 0), " of ", n)
  }

  res_std <- raw_res / sqrt(F_vec)

  v_t <- raw_res
  F_t <- F_vec
  LogLik_t <- -0.5 * (log(2 * pi * F_t) + v_t^2 / F_t)

  non_finite_mask <- !is.finite(v_t) | !is.finite(F_t) | !is.finite(LogLik_t)
  if (any(non_finite_mask)) {
    warning("Non-finite innovation values: ", sum(non_finite_mask), " of ", n)
  }

  phi1_mean <- mean(phi1_t)
  phi2_mean <- mean(phi2_t)
  ar_check <- check_ar2_stability(phi1_mean, phi2_mean)

  cat(sprintf(
    "    phi1: %.4f, phi2: %.4f, AR_denom: %.4f, AR_stable: %s, B: %d, cor(gas,oil): %.3f\n",
    phi1_mean, phi2_mean, ar_check$ar_denom,
    ifelse(ar_check$stable, "YES", "NO"), B, cor_x_o
  ))

  if (ar_check$denom_caution) {
    warning("AR denominator near zero. Long-run coefficients may be unreliable.")
  }

  # --- Regime-specific alpha summary ---
  regime_alpha_summary <- tibble(
    Regime = seq_len(K),
    Alpha = as.numeric(alpha_mat[n, ]),
    SE = sapply(alpha_idx, function(k) sqrt(vl_sm[[n]][k, k]))
  )

  omega_summary <- NULL
  if (B > 0) {
    omega_se <- sapply(omega_idx, function(j) sqrt(vl_sm[[n]][j, j]))
    omega_summary <- tibble(
      Break_num = seq_len(B),
      Omega = omega_vals,
      SE = omega_se
    )
  }

  k_hyper  <- 3L
  k_static <- K + B + 2L  # alphas + omegas + phi1 + phi2

  est_pars <- tibble(
    V_obs = exp(best_fit$par[1]) + V_FLOOR,
    Q_beta = exp(best_fit$par[2]),
    Q_delta = exp(best_fit$par[3]),

    log_V_obs = best_fit$par[1],
    log_Q_beta = best_fit$par[2],
    log_Q_delta = best_fit$par[3],

    V_near_start_grid_edge =
      best_fit$par[1] <= min(p1_grid) + 0.1 |
      best_fit$par[1] >= max(p1_grid) - 0.1,

    Q_beta_near_start_grid_edge =
      best_fit$par[2] <= min(p2_grid) + 0.1 |
      best_fit$par[2] >= max(p2_grid) - 0.1,

    Q_delta_near_start_grid_edge =
      best_fit$par[3] <= min(p3_grid) + 0.1 |
      best_fit$par[3] >= max(p3_grid) - 0.1,

    Q_beta_effectively_zero = exp(best_fit$par[2]) < 1e-8,
    Q_delta_effectively_zero = exp(best_fit$par[3]) < 1e-8,

    Phi1 = phi1_mean,
    Phi1_SE = mean(sd_phi1),
    Phi2 = phi2_mean,
    Phi2_SE = mean(sd_phi2),

    AR_Denominator = ar_check$ar_denom,
    AR_Min_Root_Modulus = ar_check$min_root_modulus,
    AR_Stable = ar_check$stable,
    AR_Denom_Caution = ar_check$denom_caution,

    Corr_gas_oil_regressors = cor_x_o,

    N_breaks = B,
    N_regimes = K,
    N_states = p,

    Neg_LogLik = best_nll,

    AIC_hyper  = ifelse(is.finite(best_nll), 2 * best_nll + 2 * k_hyper, NA_real_),
    BIC_hyper  = ifelse(is.finite(best_nll), 2 * best_nll + k_hyper * log(n), NA_real_),
    AIC_static = ifelse(is.finite(best_nll), 2 * best_nll + 2 * k_static, NA_real_),
    BIC_static = ifelse(is.finite(best_nll), 2 * best_nll + k_static * log(n), NA_real_),

    Converged = mle_converged
  )

  list(
    beta_base = beta_base,
    sd_beta_base = sd_beta_base,

    beta_eff = beta_eff,
    sd_beta_eff = sd_beta_eff,
    beta_eff_lr = beta_eff_lr,
    sd_beta_eff_lr = sd_beta_eff_lr,

    delta = delta_t,
    sd_delta = sd_delta,
    delta_lr = delta_lr_t,
    sd_delta_lr = sd_delta_lr,

    ar_denom = ar_denom_t,

    alpha_eff = alpha_eff,
    sd_alpha_eff = sd_alpha_eff,

    phi1 = phi1_t,
    sd_phi1 = sd_phi1,
    phi2 = phi2_t,
    sd_phi2 = sd_phi2,

    res_std = res_std,

    innovations = tibble(
      v_t = v_t,
      F_t = F_t,
      LogLik_t = LogLik_t
    ),

    est_pars = est_pars,
    regime_alphas = regime_alpha_summary,
    omega_summary = omega_summary,

    n_obs = n,
    ar_check = ar_check,
    cor_x_o = cor_x_o
  )
}

# Integration assessment: impact and long-run coefficients
assess_integration_dummy <- function(beta_eff, sd_beta_eff, beta_eff_lr, sd_beta_eff_lr,
                                     delta_t, sd_delta, delta_lr_t, sd_delta_lr, n) {
  mid <- floor(n / 2)
  first_half <- 1:mid
  second_half <- (mid + 1):n
  terminal_window <- max(1, n - 249):n

  b_lo <- beta_eff - 1.96 * sd_beta_eff
  b_hi <- beta_eff + 1.96 * sd_beta_eff
  b_ci_1 <- (b_lo <= 1) & (b_hi >= 1)

  lr_lo <- beta_eff_lr - 1.96 * sd_beta_eff_lr
  lr_hi <- beta_eff_lr + 1.96 * sd_beta_eff_lr
  lr_ci_1 <- !is.na(beta_eff_lr) & (lr_lo <= 1) & (lr_hi >= 1)

  tibble(
    Beta_eff_impact_mean_full =
      mean(beta_eff, na.rm = TRUE),

    Beta_eff_impact_mean_terminal =
      mean(beta_eff[terminal_window], na.rm = TRUE),

    Beta_eff_impact_pct_CI_1 =
      100 * mean(b_ci_1, na.rm = TRUE),

    Impact_convergence_shift =
      mean(beta_eff[second_half], na.rm = TRUE) -
      mean(beta_eff[first_half], na.rm = TRUE),

    Beta_eff_LR_mean_full =
      mean(beta_eff_lr, na.rm = TRUE),

    Beta_eff_LR_mean_terminal =
      mean(beta_eff_lr[terminal_window], na.rm = TRUE),

    Beta_eff_LR_pct_CI_1 =
      100 * mean(lr_ci_1, na.rm = TRUE),

    LR_convergence_shift =
      mean(beta_eff_lr[second_half], na.rm = TRUE) -
      mean(beta_eff_lr[first_half], na.rm = TRUE),

    Delta_impact_mean_full =
      mean(delta_t, na.rm = TRUE),

    Delta_impact_mean_terminal =
      mean(delta_t[terminal_window], na.rm = TRUE),

    Delta_LR_mean_full =
      mean(delta_lr_t, na.rm = TRUE),

    Delta_LR_mean_terminal =
      mean(delta_lr_t[terminal_window], na.rm = TRUE)
  )
}

# Crisis-window definitions
stress_windows <- tibble(
  Window = c(
    "Pre-COVID",
    "COVID shock",
    "Interim recovery",
    "Energy crisis (pre-war)",
    "Post-invasion crisis",
    "Normalization"
  ),
  Start = as.Date(c(
    NA,
    "2020-02-01",
    "2020-07-01",
    "2021-09-01",
    "2022-02-24",
    "2023-01-01"
  )),
  End = as.Date(c(
    "2020-01-31",
    "2020-06-30",
    "2021-08-31",
    "2022-02-23",
    "2022-12-31",
    NA
  ))
)

stress_windows$Start[1] <- min(gas$Date)
stress_windows$End[nrow(stress_windows)] <- max(gas$Date)

# Run model for each directed pair
cat("Running Dummy Kalman Filter with Spread-Based Breaks ...\n")
cat(
  "  Specification: sum_k[alpha_k*R_k] + [beta_base_t + sum_b omega_b*S_b]*log(P_ind)",
  "+ delta_t*log(Brent_MA3) + phi1*y_{t-1} + phi2*y_{t-2}\n\n"
)

path_list <- list()
res_list <- list()
diag_list <- list()
par_list <- list()
integ_list <- list()
regime_list <- list()
omega_list <- list()
innov_list <- list()
regime_count_list <- list()
resid_window_list <- list()
pacf_list <- list()

for (pair in hub_pairs) {
  dep <- pair[1]
  ind <- pair[2]
  pair_id <- paste0(dep, "_", ind)

  cat("  Pair:", dep, "<-", ind, " ... ")
  t_start <- Sys.time()

  df <- gas |>
    dplyr::select(
      Date,
      dep_raw = all_of(dep),
      ind_raw = all_of(ind),
      oil_raw = Brent_MA3
    ) |>
    filter(dep_raw > 0, ind_raw > 0, oil_raw > 0) |>
    drop_na() |>
    mutate(
      y = log(dep_raw),
      x = log(ind_raw),
      o = log(oil_raw)
    )

  if (nrow(df) < 100) {
    cat("skipped\n")
    next
  }

  dum_info <- build_dummies(df$Date, dep, ind, breaks_raw)

  n_raw <- nrow(df)

  y_lag1 <- df$y[2:(n_raw - 1)]
  y_lag2 <- df$y[1:(n_raw - 2)]

  y <- df$y[3:n_raw]
  x <- df$x[3:n_raw]
  o <- df$o[3:n_raw]
  dates <- df$Date[3:n_raw]

  R_mat <- dum_info$R_mat[3:n_raw, , drop = FALSE]
  S_mat <- if (!is.null(dum_info$S_mat)) {
    dum_info$S_mat[3:n_raw, , drop = FALSE]
  } else {
    NULL
  }

  # --- Regime observation counts ---
  regime_est <- dum_info$regime[3:n_raw]

  for (k in seq_len(dum_info$K)) {
    regime_count_list[[paste0(pair_id, "_R", k)]] <- tibble(
      Dependent = dep,
      Independent = ind,
      Regime = k,
      N_obs = sum(regime_est == k)
    )
  }

  # --- Run Kalman filter ---
  out <- run_kalman_regime_dummies_oil(
    y = y,
    x = x,
    o = o,
    y_lag1 = y_lag1,
    y_lag2 = y_lag2,
    R_mat = R_mat,
    S_mat = S_mat
  )

  elapsed <- round(difftime(Sys.time(), t_start, units = "secs"), 1)
  cat("  done (", elapsed, "s, n=", out$n_obs, ")\n")

  # --- State paths ---
  path_list[[pair_id]] <- tibble(
    Dependent = dep,
    Independent = ind,
    Date = dates,

    Alpha = out$alpha_eff,
    Alpha_lower = out$alpha_eff - 1.96 * out$sd_alpha_eff,
    Alpha_upper = out$alpha_eff + 1.96 * out$sd_alpha_eff,

    Beta_base = out$beta_base,
    Beta_base_lower = out$beta_base - 1.96 * out$sd_beta_base,
    Beta_base_upper = out$beta_base + 1.96 * out$sd_beta_base,

    Beta_eff = out$beta_eff,
    Beta_eff_lower = out$beta_eff - 1.96 * out$sd_beta_eff,
    Beta_eff_upper = out$beta_eff + 1.96 * out$sd_beta_eff,

    AR_Denominator = out$ar_denom,

    Beta_eff_LongRun = out$beta_eff_lr,
    Beta_eff_LongRun_lower_approx = out$beta_eff_lr - 1.96 * out$sd_beta_eff_lr,
    Beta_eff_LongRun_upper_approx = out$beta_eff_lr + 1.96 * out$sd_beta_eff_lr,

    Delta = out$delta,
    Delta_lower = out$delta - 1.96 * out$sd_delta,
    Delta_upper = out$delta + 1.96 * out$sd_delta,

    Delta_LongRun = out$delta_lr,
    Delta_LongRun_lower_approx = out$delta_lr - 1.96 * out$sd_delta_lr,
    Delta_LongRun_upper_approx = out$delta_lr + 1.96 * out$sd_delta_lr,

    Phi1 = out$phi1,
    Phi2 = out$phi2
  )

  # --- Residuals ---
  res_list[[pair_id]] <- tibble(
    Dependent = dep,
    Independent = ind,
    Date = dates,
    Residual_Std = out$res_std
  )

  # --- Innovations ---
  innov_list[[pair_id]] <- tibble(
    Dependent = dep,
    Independent = ind,
    Date = dates,
    v_t = out$innovations$v_t,
    F_t = out$innovations$F_t,
    LogLik_t = out$innovations$LogLik_t
  )

  # --- Parameter outputs ---
  par_list[[pair_id]] <- out$est_pars |>
    mutate(
      Dependent = dep,
      Independent = ind,
      N = out$n_obs
    )

  # --- Regime alphas ---
  regime_list[[pair_id]] <- out$regime_alphas |>
    mutate(
      Dependent = dep,
      Independent = ind
    )

  # --- Dummy coefficients ---
  if (!is.null(out$omega_summary)) {
    omega_list[[pair_id]] <- out$omega_summary |>
      mutate(
        Dependent = dep,
        Independent = ind,
        Break_date = dum_info$break_dates[Break_num]
      )
  }

  # --- Integration tests ---
  integ_list[[pair_id]] <- assess_integration_dummy(
    beta_eff = out$beta_eff,
    sd_beta_eff = out$sd_beta_eff,
    beta_eff_lr = out$beta_eff_lr,
    sd_beta_eff_lr = out$sd_beta_eff_lr,
    delta_t = out$delta,
    sd_delta = out$sd_delta,
    delta_lr_t = out$delta_lr,
    sd_delta_lr = out$sd_delta_lr,
    n = out$n_obs
  ) |>
    mutate(
      Dependent = dep,
      Independent = ind,
      AR_Denominator = out$ar_check$ar_denom,
      AR_Stable = out$ar_check$stable,
      AR_Denom_Caution = out$ar_check$denom_caution,
      Corr_gas_oil_regressors = out$cor_x_o
    )

  # --- Residual diagnostics ---
  clean_res <- out$res_std[is.finite(out$res_std)]

  if (length(clean_res) > 30) {
    lb <- Box.test(clean_res, lag = 20, type = "Ljung-Box")
    lb2 <- Box.test(clean_res^2, lag = 20, type = "Ljung-Box")
    jb <- tseries::jarque.bera.test(clean_res)

    diag_list[[pair_id]] <- tibble(
      Dependent = dep,
      Independent = ind,
      LB_stat = as.numeric(lb$statistic),
      LB_p = lb$p.value,
      LB_sq_stat = as.numeric(lb2$statistic),
      LB_sq_p = lb2$p.value,
      JB_stat = as.numeric(jb$statistic),
      JB_p = jb$p.value
    )
  }

  # --- PACF diagnostics ---
  if (length(clean_res) > 35) {
    pacf_obj <- pacf(clean_res, lag.max = 35, plot = FALSE)
    pacf_list[[pair_id]] <- tibble(
      Dependent = dep,
      Independent = ind,
      Lag = 1:35,
      PACF = pacf_obj$acf[1:35],
      Significant = abs(pacf_obj$acf[1:35]) > 1.96 / sqrt(length(clean_res))
    )
  }

  # --- Crisis-window residual diagnostics ---
  res_df <- tibble(
    Date = dates,
    Residual_Std = out$res_std
  )

  for (w in seq_len(nrow(stress_windows))) {
    sw <- stress_windows[w, ]

    sub <- res_df |>
      filter(Date >= sw$Start, Date <= sw$End)

    if (nrow(sub) < 10) next

    clean_sub <- sub$Residual_Std[is.finite(sub$Residual_Std)]
    if (length(clean_sub) < 10) next

    lb_lag <- min(20, length(clean_sub) - 5)

    lb_w <- Box.test(clean_sub, lag = lb_lag, type = "Ljung-Box")
    lb2_w <- Box.test(clean_sub^2, lag = lb_lag, type = "Ljung-Box")

    resid_window_list[[paste0(pair_id, "_", w)]] <- tibble(
      Dependent = dep,
      Independent = ind,
      Window = sw$Window,
      N = nrow(sub),
      SD_residual = sd(clean_sub),
      Mean_abs_residual = mean(abs(clean_sub)),
      LB_p = lb_w$p.value,
      LB_sq_p = lb2_w$p.value
    )
  }
}

# Build undirected pair integration summary
cat("\nBuilding pair integration summary...\n")

integ_all <- if (length(integ_list) > 0) {
  bind_rows(integ_list)
} else {
  tibble()
}

pair_summary_list <- list()
gas_pairs <- combn(hubs, 2, simplify = FALSE)

for (pr in gas_pairs) {
  h1 <- pr[1]
  h2 <- pr[2]

  fwd <- integ_all |>
    filter(Dependent == h1, Independent == h2)

  rev <- integ_all |>
    filter(Dependent == h2, Independent == h1)

  if (nrow(fwd) == 0 || nrow(rev) == 0) next

  pair_summary_list[[paste0(h1, "_", h2)]] <- tibble(
    Pair = paste(h1, "-", h2),

    Beta_eff_LR_terminal_fwd = fwd$Beta_eff_LR_mean_terminal,
    Beta_eff_LR_terminal_rev = rev$Beta_eff_LR_mean_terminal,

    LR_CI_1_fwd = fwd$Beta_eff_LR_pct_CI_1,
    LR_CI_1_rev = rev$Beta_eff_LR_pct_CI_1,

    Delta_LR_terminal_fwd = fwd$Delta_LR_mean_terminal,
    Delta_LR_terminal_rev = rev$Delta_LR_mean_terminal,

    AR_Stable_fwd = fwd$AR_Stable,
    AR_Stable_rev = rev$AR_Stable,

    Corr_gas_oil_fwd = fwd$Corr_gas_oil_regressors,
    Corr_gas_oil_rev = rev$Corr_gas_oil_regressors,

    Mean_abs_LR_gap =
      abs(fwd$Beta_eff_LR_mean_terminal - rev$Beta_eff_LR_mean_terminal),

    LR_near_1_threshold = 0.3,

    Both_LR_near_1 =
      abs(fwd$Beta_eff_LR_mean_terminal - 1) < 0.3 &
      abs(rev$Beta_eff_LR_mean_terminal - 1) < 0.3,

    Both_AR_stable =
      fwd$AR_Stable & rev$AR_Stable,

    Note = "LR_near_1_threshold is a diagnostic heuristic. LR CIs are approximate."
  )
}

pair_summary <- if (length(pair_summary_list) > 0) {
  bind_rows(pair_summary_list)
} else {
  tibble()
}

# Save outputs

if (length(path_list) > 0) {
  write_csv(bind_rows(path_list), file.path(kf_dir, "state_paths.csv"))
}

if (length(res_list) > 0) {
  write_csv(bind_rows(res_list), file.path(kf_dir, "standardised_residuals.csv"))
}

if (length(innov_list) > 0) {
  write_csv(bind_rows(innov_list), file.path(kf_dir, "innovations.csv"))
}

if (length(par_list) > 0) {
  write_csv(bind_rows(par_list), file.path(kf_dir, "mle_parameters.csv"))
}

if (length(integ_list) > 0) {
  write_csv(bind_rows(integ_list), file.path(kf_dir, "integration_tests.csv"))
}

if (length(diag_list) > 0) {
  write_csv(bind_rows(diag_list), file.path(kf_dir, "model_diagnostics.csv"))
}

if (length(regime_list) > 0) {
  write_csv(bind_rows(regime_list), file.path(kf_dir, "regime_alphas.csv"))
}

if (length(omega_list) > 0) {
  write_csv(bind_rows(omega_list), file.path(kf_dir, "dummy_coefficients.csv"))
}

if (length(regime_count_list) > 0) {
  write_csv(bind_rows(regime_count_list), file.path(kf_dir, "regime_observation_counts.csv"))
}

if (nrow(pair_summary) > 0) {
  write_csv(pair_summary, file.path(kf_dir, "integration_summary_by_pair.csv"))
}

if (length(resid_window_list) > 0) {
  write_csv(bind_rows(resid_window_list), file.path(kf_dir, "residuals_by_window.csv"))
}
if (length(pacf_list) > 0) {
  write_csv(bind_rows(pacf_list), file.path(kf_dir, "pacf_diagnostics.csv"))
}
cat("\n== Dummy-augmented Kalman Filter  Finished ==\n")
cat("   Output files written to:\n   ", kf_dir, "\n")