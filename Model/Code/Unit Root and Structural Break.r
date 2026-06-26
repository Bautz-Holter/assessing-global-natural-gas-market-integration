# Unit Root & Structural Break Tests (Refactored)
# Input: gas_clean_ma3_balanced.csv
# Output: Unit root battery with consensus classification, spec-matched structural breaks

pkgs <- c(
  "dplyr", "readr", "tidyr", "tibble", "urca",
  "tseries", "strucchange", "here"
)
to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

project_dir <- here::here()

input_file <- file.path(
  project_dir, "Model", "model output",
  "Data Cleaning", "gas_clean_ma3_balanced.csv"
)

bt_dir <- file.path(
  project_dir, "Model", "model output",
  "Unit Root and Structural Break"
)

ur_dir    <- file.path(bt_dir, "Unit_Root")
bp_dir    <- file.path(bt_dir, "Bai_Perron")
sp_dir    <- file.path(bt_dir, "Pairwise_Spreads")
ba_dir    <- file.path(bt_dir, "Break_Alignment")
rob_dir   <- file.path(bt_dir, "Robustness")

invisible(lapply(
  c(ur_dir, bp_dir, sp_dir, ba_dir, rob_dir),
  function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
))

if (!file.exists(input_file)) stop("Input file missing: ", input_file)

gas <- read_csv(input_file, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

gas_hubs <- c("JKM", "NBP", "TTF", "HH")
oil_hub  <- "Brent_MA3"
price_hubs <- c(gas_hubs, oil_hub)

required_cols <- c("Date", price_hubs)

missing_cols <- setdiff(required_cols, names(gas))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

if (anyNA(gas[price_hubs])) {
  stop("Input contains missing values. Re-run Data Cleaning.")
}

if (any(gas[price_hubs] <= 0, na.rm = TRUE)) {
  stop("Non-positive prices found. Log transformations are unsafe.")
}

cat(sprintf(
  "Sample: %s to %s (%d MA3-balanced observations)\n",
  min(gas$Date), max(gas$Date), nrow(gas)
))

# Apply log transformations
gas <- gas |>
  mutate(across(all_of(price_hubs), log, .names = "{.col}_log"))

# Compute returns
returns <- gas |>
  mutate(across(
    all_of(price_hubs),
    ~ log(.x / dplyr::lag(.x)),
    .names = "{.col}_ret"
  ))


# --- helper functions ---

bic_n_breaks <- function(bic_val) {
  valid_bic <- bic_val[is.finite(bic_val)]

  if (length(valid_bic) == 0) return(0L)

  nm <- names(valid_bic)
  idx <- which.min(valid_bic)

  if (!is.null(nm) && !is.na(nm[idx]) && nzchar(nm[idx])) {
    out <- suppressWarnings(as.integer(nm[idx]))
  } else {
    out <- idx - 1L
  }

  if (length(out) == 0 || is.na(out)) out <- 0L
  as.integer(out)
}

bic_min <- function(bic_val) {
  valid_bic <- bic_val[is.finite(bic_val)]
  if (length(valid_bic) == 0) return(NA_real_)
  min(valid_bic, na.rm = TRUE)
}

run_adf_urca <- function(x, type = "drift", max_lag = 20) {
  x <- na.omit(as.numeric(x))

  if (length(x) < 50) {
    return(tibble(
      ADF_stat = NA_real_,
      ADF_1pct = NA_real_,
      ADF_5pct = NA_real_,
      ADF_10pct = NA_real_,
      ADF_lag = NA_integer_,
      ADF_reject_5pct = NA
    ))
  }

  obj <- tryCatch(
    urca::ur.df(x, type = type, selectlags = "AIC", lags = max_lag),
    error = function(e) NULL
  )

  if (is.null(obj)) {
    return(tibble(
      ADF_stat = NA_real_,
      ADF_1pct = NA_real_,
      ADF_5pct = NA_real_,
      ADF_10pct = NA_real_,
      ADF_lag = NA_integer_,
      ADF_reject_5pct = NA
    ))
  }

  stat  <- obj@teststat[1, 1]
  cvals <- obj@cval[1, ]

  tibble(
    ADF_stat = as.numeric(stat),
    ADF_1pct = as.numeric(cvals[1]),
    ADF_5pct = as.numeric(cvals[2]),
    ADF_10pct = as.numeric(cvals[3]),
    ADF_lag = obj@lags,
    ADF_reject_5pct = as.numeric(stat) < as.numeric(cvals[2])
  )
}

run_pp <- function(x) {
  x <- na.omit(as.numeric(x))

  if (length(x) < 20) {
    return(tibble(
      PP_stat = NA_real_,
      PP_p = NA_real_,
      PP_reject_5pct = NA
    ))
  }

  pp <- tryCatch(
    suppressWarnings(tseries::pp.test(x)),
    error = function(e) NULL
  )

  if (is.null(pp)) {
    return(tibble(
      PP_stat = NA_real_,
      PP_p = NA_real_,
      PP_reject_5pct = NA
    ))
  }

  tibble(
    PP_stat = as.numeric(pp$statistic),
    PP_p = pp$p.value,
    PP_reject_5pct = pp$p.value < 0.05
  )
}

run_kpss <- function(x, null = "Level") {
  x <- na.omit(as.numeric(x))

  if (length(x) < 20) {
    return(tibble(
      KPSS_stat = NA_real_,
      KPSS_p = NA_real_,
      KPSS_reject_5pct = NA
    ))
  }

  kp <- tryCatch(
    suppressWarnings(tseries::kpss.test(x, null = null)),
    error = function(e) NULL
  )

  if (is.null(kp)) {
    return(tibble(
      KPSS_stat = NA_real_,
      KPSS_p = NA_real_,
      KPSS_reject_5pct = NA
    ))
  }

  tibble(
    KPSS_stat = as.numeric(kp$statistic),
    KPSS_p = kp$p.value,
    KPSS_reject_5pct = kp$p.value < 0.05
  )
}

run_za_intercept <- function(x, dates = NULL, max_lag = 20) {
  x <- na.omit(as.numeric(x))

  if (length(x) < 50) {
    return(tibble(
      ZA_intercept_stat = NA_real_,
      ZA_intercept_5pct = NA_real_,
      ZA_intercept_lag = NA_integer_,
      ZA_intercept_reject_5pct = NA,
      ZA_intercept_break_index = NA_integer_,
      ZA_intercept_break_date = as.Date(NA)
    ))
  }

  best_lag <- run_adf_urca(x, type = "drift")$ADF_lag
  if (is.na(best_lag)) best_lag <- 4L

  za_int <- tryCatch(
    urca::ur.za(x, model = "intercept", lag = best_lag),
    error = function(e) NULL
  )

  if (is.null(za_int)) {
    return(tibble(
      ZA_intercept_stat = NA_real_,
      ZA_intercept_5pct = NA_real_,
      ZA_intercept_lag = NA_integer_,
      ZA_intercept_reject_5pct = NA,
      ZA_intercept_break_index = NA_integer_,
      ZA_intercept_break_date = as.Date(NA)
    ))
  }

  tau <- as.numeric(za_int@teststat[1])

  cval_raw <- za_int@cval
  cv5 <- if (is.null(dim(cval_raw))) {
    pos <- if (!is.null(names(cval_raw))) grep("5", names(cval_raw)) else integer(0)
    if (length(pos) > 0) as.numeric(cval_raw[pos[1]]) else if (length(cval_raw) >= 2) as.numeric(cval_raw[2]) else NA_real_
  } else {
    pos <- if (!is.null(colnames(cval_raw))) grep("5", colnames(cval_raw)) else integer(0)
    if (length(pos) > 0) as.numeric(cval_raw[1, pos[1]]) else if (ncol(cval_raw) >= 2) as.numeric(cval_raw[1, 2]) else NA_real_
  }

  bkpt <- za_int@bpoint
  bk_date <- if (!is.na(bkpt) && bkpt <= length(x) && !is.null(dates) && bkpt <= length(dates)) {
    as.Date(dates[bkpt])
  } else {
    as.Date(NA)
  }

  tibble(
    ZA_intercept_stat = tau,
    ZA_intercept_5pct = cv5,
    ZA_intercept_lag = as.integer(best_lag),
    ZA_intercept_reject_5pct = tau < cv5,
    ZA_intercept_break_index = as.integer(bkpt),
    ZA_intercept_break_date = bk_date
  )
}

run_za_trend <- function(x, dates = NULL, max_lag = 20) {
  x <- na.omit(as.numeric(x))

  if (length(x) < 50) {
    return(tibble(
      ZA_trend_stat = NA_real_,
      ZA_trend_5pct = NA_real_,
      ZA_trend_lag = NA_integer_,
      ZA_trend_reject_5pct = NA,
      ZA_trend_break_index = NA_integer_,
      ZA_trend_break_date = as.Date(NA)
    ))
  }

  best_lag <- run_adf_urca(x, type = "drift")$ADF_lag
  if (is.na(best_lag)) best_lag <- 4L

  za_trend <- tryCatch(
    urca::ur.za(x, model = "trend", lag = best_lag),
    error = function(e) NULL
  )

  if (is.null(za_trend)) {
    return(tibble(
      ZA_trend_stat = NA_real_,
      ZA_trend_5pct = NA_real_,
      ZA_trend_lag = NA_integer_,
      ZA_trend_reject_5pct = NA,
      ZA_trend_break_index = NA_integer_,
      ZA_trend_break_date = as.Date(NA)
    ))
  }

  tau <- as.numeric(za_trend@teststat[1])

  cval_raw <- za_trend@cval
  cv5 <- if (is.null(dim(cval_raw))) {
    pos <- if (!is.null(names(cval_raw))) grep("5", names(cval_raw)) else integer(0)
    if (length(pos) > 0) as.numeric(cval_raw[pos[1]]) else if (length(cval_raw) >= 2) as.numeric(cval_raw[2]) else NA_real_
  } else {
    pos <- if (!is.null(colnames(cval_raw))) grep("5", colnames(cval_raw)) else integer(0)
    if (length(pos) > 0) as.numeric(cval_raw[1, pos[1]]) else if (ncol(cval_raw) >= 2) as.numeric(cval_raw[1, 2]) else NA_real_
  }

  bkpt <- za_trend@bpoint
  bk_date <- if (!is.na(bkpt) && bkpt <= length(x) && !is.null(dates) && bkpt <= length(dates)) {
    as.Date(dates[bkpt])
  } else {
    as.Date(NA)
  }

  tibble(
    ZA_trend_stat = tau,
    ZA_trend_5pct = cv5,
    ZA_trend_lag = as.integer(best_lag),
    ZA_trend_reject_5pct = tau < cv5,
    ZA_trend_break_index = as.integer(bkpt),
    ZA_trend_break_date = bk_date
  )
}

nearest_date <- function(target_date, candidate_dates) {
  candidate_dates <- as.Date(candidate_dates)
  candidate_dates <- candidate_dates[!is.na(candidate_dates)]

  if (length(candidate_dates) == 0 || is.na(target_date)) {
    return(as.Date(NA))
  }

  candidate_dates[
    which.min(abs(as.numeric(difftime(candidate_dates, target_date, units = "days"))))
  ]
}


# 1. Unit root battery — log price levels

cat("Unit root battery: log price levels\n")

ur_price_rows <- list()

for (h in price_hubs) {
  cat("   Series:", h, "\n")

  x_log <- gas[[paste0(h, "_log")]]

  # ADF with drift
  adf_drift <- run_adf_urca(x_log, type = "drift")

  # Phillips Perron
  pp_result <- run_pp(x_log)

  # KPSS with level null
  kpss_result <- run_kpss(x_log, null = "Level")

  # Zivot-Andrews intercept model
  za_int <- run_za_intercept(x_log, dates = gas$Date)

  # Zivot-Andrews trend model (intercept and trend)
  za_trend <- run_za_trend(x_log, dates = gas$Date)

  ur_price_rows[[h]] <- bind_cols(
    tibble(Series = h, Type = "Price_Level"),
    adf_drift,
    pp_result,
    kpss_result,
    za_int,
    za_trend
  )
}

ur_prices <- bind_rows(ur_price_rows)
write_csv(ur_prices, file.path(ur_dir, "unit_root_prices.csv"))


# 2. Unit root battery — gas-gas spreads (unity coefficient)

cat("Unit root battery: gas-gas spreads\n")

spread_pairs <- list()
for (i in seq_along(gas_hubs)) {
  for (j in seq_along(gas_hubs)) {
    if (i >= j) next
    spread_pairs[[length(spread_pairs) + 1]] <- list(hub1 = gas_hubs[i], hub2 = gas_hubs[j])
  }
}

ur_spread_rows <- list()

for (pair in spread_pairs) {
  h1 <- pair$hub1
  h2 <- pair$hub2
  pair_name <- paste(h1, h2, sep = "_")

  cat("   Spread:", h1, "-", h2, "\n")

  # Spread with unit coefficient: log(h1) - log(h2)
  spread <- gas[[paste0(h1, "_log")]] - gas[[paste0(h2, "_log")]]

  # For spreads: ADF with drift, PP, KPSS, ZA intercept only
  adf_drift <- run_adf_urca(spread, type = "drift")
  pp_result <- run_pp(spread)
  kpss_result <- run_kpss(spread, null = "Level")
  za_int <- run_za_intercept(spread, dates = gas$Date)

  ur_spread_rows[[pair_name]] <- bind_cols(
    tibble(
      Pair = paste(h1, "-", h2),
      Hub_A = h1,
      Hub_B = h2,
      Type = "Gas_Gas_Spread",
      Specification = "beta = 1 unity coeff"
    ),
    adf_drift,
    pp_result,
    kpss_result,
    za_int
  )
}

ur_spreads <- bind_rows(ur_spread_rows)
write_csv(ur_spreads, file.path(ur_dir, "unit_root_spreads.csv"))


## Three-way consensus classification (log price levels)

cat("Three-way consensus classification\n")

consensus_rows <- list()

for (i in seq_len(nrow(ur_prices))) {
  r <- ur_prices[i, ]

  series_name <- r$Series

  # Baseline tests (ADF with drift or PP)
  adf_rejects <- isTRUE(r$ADF_reject_5pct)
  pp_rejects <- isTRUE(r$PP_reject_5pct)
  baseline_rejects <- adf_rejects | pp_rejects

  # Break-aware tests (Zivot-Andrews)
  za_int_rejects <- isTRUE(r$ZA_intercept_reject_5pct)
  za_trend_rejects <- isTRUE(r$ZA_trend_reject_5pct)
  break_tests_reject <- za_int_rejects | za_trend_rejects

  # Three-way classification
  if (baseline_rejects) {
    integration_order <- "I(0)"
    classification <- "Space_1_Stationary"
    interpretation <- "Baseline tests reject unit root null"
  } else if (!baseline_rejects & break_tests_reject) {
    integration_order <- "TS"
    classification <- "Space_2_Stationary_with_Break"
    interpretation <- "Baseline tests fail to reject, but Zivot-Andrews rejects allowing endogenous break"
  } else {
    integration_order <- "I(1)"
    classification <- "Space_3_Non_Stationary"
    interpretation <- "Neither baseline nor break-aware tests reject unit root null"
  }

  consensus_rows[[series_name]] <- tibble(
    Series = series_name,
    ADF_reject_5pct = adf_rejects,
    PP_reject_5pct = pp_rejects,
    ZA_intercept_reject_5pct = za_int_rejects,
    ZA_trend_reject_5pct = za_trend_rejects,
    Integration_Order = integration_order,
    Classification = classification,
    Interpretation = interpretation,
    ZA_intercept_break_date = r$ZA_intercept_break_date,
    ZA_trend_break_date = r$ZA_trend_break_date
  )
}

consensus_table <- bind_rows(consensus_rows)
write_csv(consensus_table, file.path(ur_dir, "three_way_consensus_classification.csv"))


# 4. Bai-Perron structural breaks — log price levels (intercept + trend)

cat("Bai-Perron: log price levels\n")

bp_price_rows   <- list()
bp_price_bic    <- list()

for (h in price_hubs) {
  cat("   Bai-Perron for:", h, "\n")

  y <- gas[[paste0(h, "_log")]]
  d <- gas$Date
  n <- length(y)

  if (n < 50) next

  trend <- seq_along(y)

  # Spec: intercept and trend (appropriate for log price levels)
  bp <- tryCatch(
    strucchange::breakpoints(y ~ 1 + trend, h = 0.15),
    error = function(e) NULL
  )

  if (is.null(bp)) next

  bp_sum  <- summary(bp)
  bic_val <- bp_sum$RSS["BIC", ]

  n_breaks_bic <- bic_n_breaks(bic_val)
  bic_selected <- bic_min(bic_val)

  bp_price_bic[[h]] <- tibble(
    Series = h,
    Specification = "intercept + trend",
    Breaks_BIC = n_breaks_bic,
    BIC_value = bic_selected
  )

  if (n_breaks_bic > 0) {
    b_idx <- tryCatch(
      strucchange::breakpoints(bp, breaks = n_breaks_bic)$breakpoints,
      error = function(e) NA_integer_
    )

    if (any(!is.na(b_idx))) {
      b_idx <- b_idx[!is.na(b_idx)]

      ci <- tryCatch(
        confint(bp, breaks = n_breaks_bic),
        error = function(e) NULL
      )

      for (i in seq_along(b_idx)) {
        row_i <- tibble(
          Series = h,
          Break_num = i,
          Break_date = d[b_idx[i]],
          Break_index = b_idx[i],
          Specification = "intercept + trend"
        )

        if (!is.null(ci)) {
          ci_mat <- ci$confint
          if (nrow(ci_mat) >= i) {
            row_i$CI_lower <- d[ci_mat[i, 1]]
            row_i$CI_upper <- d[ci_mat[i, 3]]
            row_i$CI_width_days <- as.integer(
              difftime(d[ci_mat[i, 3]], d[ci_mat[i, 1]], units = "days")
            )
          }
        }

        bp_price_rows[[paste0(h, "_", i)]] <- row_i
      }
    }
  }
}

bp_prices_tbl <- if (length(bp_price_rows) > 0) bind_rows(bp_price_rows) else tibble()
bp_prices_bic <- if (length(bp_price_bic) > 0) bind_rows(bp_price_bic) else tibble()

write_csv(bp_prices_tbl, file.path(bp_dir, "BaiPerron_break_dates.csv"))
write_csv(bp_prices_bic, file.path(bp_dir, "BaiPerron_BIC_selection.csv"))



# 5. Bai-Perron structural breaks — gas-gas spreads (intercept only)

cat("Bai-Perron: gas-gas spreads\n")

bp_spread_rows   <- list()
bp_spread_bic    <- list()

for (pair in spread_pairs) {
  h1 <- pair$hub1
  h2 <- pair$hub2
  pair_name <- paste(h1, h2, sep = "_")

  cat("   Bai-Perron for:", h1, "-", h2, "\n")

  y <- gas[[paste0(h1, "_log")]] - gas[[paste0(h2, "_log")]]
  d <- gas$Date
  n <- length(y)

  if (n < 50) next

  # Spec: intercept only (appropriate for spreads)
  bp <- tryCatch(
    strucchange::breakpoints(y ~ 1, h = 0.15),
    error = function(e) NULL
  )

  if (is.null(bp)) next

  bp_sum  <- summary(bp)
  bic_val <- bp_sum$RSS["BIC", ]

  n_breaks_bic <- bic_n_breaks(bic_val)
  bic_selected <- bic_min(bic_val)

  bp_spread_bic[[pair_name]] <- tibble(
    Pair = paste(h1, "-", h2),
    Hub_A = h1,
    Hub_B = h2,
    Specification = "intercept only",
    Breaks_BIC = n_breaks_bic,
    BIC_value = bic_selected
  )

  if (n_breaks_bic > 0) {
    b_idx <- tryCatch(
      strucchange::breakpoints(bp, breaks = n_breaks_bic)$breakpoints,
      error = function(e) NA_integer_
    )

    if (any(!is.na(b_idx))) {
      b_idx <- b_idx[!is.na(b_idx)]

      ci <- tryCatch(
        confint(bp, breaks = n_breaks_bic),
        error = function(e) NULL
      )

      for (i in seq_along(b_idx)) {
        row_i <- tibble(
          Pair = paste(h1, "-", h2),
          Hub_A = h1,
          Hub_B = h2,
          Break_num = i,
          Break_date = d[b_idx[i]],
          Break_index = b_idx[i],
          Specification = "intercept only"
        )

        if (!is.null(ci)) {
          ci_mat <- ci$confint
          if (nrow(ci_mat) >= i) {
            row_i$CI_lower <- d[ci_mat[i, 1]]
            row_i$CI_upper <- d[ci_mat[i, 3]]
            row_i$CI_width_days <- as.integer(
              difftime(d[ci_mat[i, 3]], d[ci_mat[i, 1]], units = "days")
            )
          }
        }

        bp_spread_rows[[paste0(h1, "_", h2, "_", i)]] <- row_i
      }
    }
  }
}

bp_spreads_tbl <- if (length(bp_spread_rows) > 0) bind_rows(bp_spread_rows) else tibble()
bp_spreads_bic <- if (length(bp_spread_bic) > 0) bind_rows(bp_spread_bic) else tibble()

write_csv(bp_spreads_tbl, file.path(sp_dir, "spread_BaiPerron_breaks.csv"))
write_csv(bp_spreads_bic, file.path(sp_dir, "spread_BaiPerron_BIC.csv"))



# 6. Regime means by Bai-Perron segment

cat("Computing regime means\n")

regime_summary <- list()

for (h in price_hubs) {
  log_col <- paste0(h, "_log")

  df <- gas |>
    select(Date, Log_Price = all_of(log_col))

  if (nrow(df) < 10) next

  breaks_h <- bp_prices_tbl |>
    filter(Series == h) |>
    pull(Break_date) |>
    as.Date()

  valid_breaks <- breaks_h[
    breaks_h > min(df$Date) & breaks_h < max(df$Date)
  ]

  regime <- rep(1L, nrow(df))

  for (b in seq_along(valid_breaks)) {
    regime[df$Date >= valid_breaks[b]] <- b + 1L
  }

  df$Regime <- regime

  df <- df |>
    group_by(Regime) |>
    mutate(Regime_Mean = mean(Log_Price)) |>
    ungroup() |>
    mutate(Series = h, N_breaks = length(valid_breaks))

  seg_summary <- df |>
    group_by(Regime) |>
    summarise(
      Start = min(Date),
      End = max(Date),
      N_obs = n(),
      Mean_log = mean(Log_Price),
      SD_log = sd(Log_Price),
      .groups = "drop"
    ) |>
    arrange(Start) |>
    mutate(
      Series = h,
      Mean_price = exp(Mean_log),
      Shift_log = Mean_log - dplyr::lag(Mean_log),
      Shift_pct = (exp(Shift_log) - 1) * 100
    ) |>
    select(
      Series, Regime, Start, End, N_obs,
      Mean_log, SD_log, Mean_price, Shift_log, Shift_pct
    )

  regime_summary[[h]] <- seg_summary
}

if (length(regime_summary) > 0) {
  write_csv(
    bind_rows(regime_summary),
    file.path(bp_dir, "BaiPerron_regime_summary.csv")
  )
}



# 7. Break date alignment — spread breaks vs marginal series

cat("Break date alignment\n")

alignment_rows <- list()

if (nrow(bp_spreads_tbl) > 0 && nrow(bp_prices_tbl) > 0) {
  for (r in seq_len(nrow(bp_spreads_tbl))) {
    row <- bp_spreads_tbl[r, ]

    sp_date <- row$Break_date

    hub_a_breaks <- bp_prices_tbl |>
      filter(Series == row$Hub_A) |>
      pull(Break_date) |>
      as.Date()

    hub_b_breaks <- bp_prices_tbl |>
      filter(Series == row$Hub_B) |>
      pull(Break_date) |>
      as.Date()

    nearest_a <- nearest_date(sp_date, hub_a_breaks)
    nearest_b <- nearest_date(sp_date, hub_b_breaks)

    days_a <- if (is.na(nearest_a)) {
      NA_integer_
    } else {
      as.integer(difftime(nearest_a, sp_date, units = "days"))
    }

    days_b <- if (is.na(nearest_b)) {
      NA_integer_
    } else {
      as.integer(difftime(nearest_b, sp_date, units = "days"))
    }

    alignment_rows[[r]] <- tibble(
      Pair = row$Pair,
      Hub_A = row$Hub_A,
      Hub_B = row$Hub_B,
      Spread_break_num = row$Break_num,
      Spread_break_date = sp_date,
      Nearest_Hub_A_break = nearest_a,
      Days_to_Hub_A_break = days_a,
      Nearest_Hub_B_break = nearest_b,
      Days_to_Hub_B_break = days_b
    )
  }
}

alignment_tbl <- if (length(alignment_rows) > 0) {
  bind_rows(alignment_rows)
} else {
  tibble()
}

write_csv(
  alignment_tbl,
  file.path(ba_dir, "break_date_alignment.csv")
)



# ---- CUSUM/MOSUM stability tests — log price levels ----

cat("CUSUM and MOSUM: log price levels\n")

cusum_price_rows <- list()

for (h in price_hubs) {
  cat("   CUSUM for:", h, "\n")

  y <- gas[[paste0(h, "_log")]]
  if (length(y) < 50) next

  df_c <- data.frame(
    y = y,
    trend = seq_along(y)
  )

  cusum_test <- tryCatch({
    ef <- strucchange::efp(y ~ 1 + trend, data = df_c, type = "OLS-CUSUM")
    sc <- strucchange::sctest(ef)

    tibble(
      Test = "OLS-CUSUM",
      Statistic = as.numeric(sc$statistic),
      p_value = sc$p.value
    )
  }, error = function(e) {
    tibble(Test = "OLS-CUSUM", Statistic = NA_real_, p_value = NA_real_)
  })

  mosum_test <- tryCatch({
    ef <- strucchange::efp(y ~ 1 + trend, data = df_c, type = "OLS-MOSUM")
    sc <- strucchange::sctest(ef)

    tibble(
      Test = "OLS-MOSUM",
      Statistic = as.numeric(sc$statistic),
      p_value = sc$p.value
    )
  }, error = function(e) {
    tibble(Test = "OLS-MOSUM", Statistic = NA_real_, p_value = NA_real_)
  })

  cusum_price_rows[[h]] <- bind_rows(cusum_test, mosum_test) |>
    mutate(
      Series = h,
      Specification = "intercept + trend",
      Reject_5pct = p_value < 0.05,
      .before = 1
    )
}

cusum_prices <- if (length(cusum_price_rows) > 0) {
  bind_rows(cusum_price_rows)
} else {
  tibble()
}

write_csv(cusum_prices, file.path(rob_dir, "CUSUM_stability.csv"))



# ---- CUSUM/MOSUM stability tests — gas-gas spreads ----

cat("CUSUM and MOSUM: gas-gas spreads\n")

cusum_spread_rows <- list()

for (pair in spread_pairs) {
  h1 <- pair$hub1
  h2 <- pair$hub2
  pair_name <- paste(h1, h2, sep = "_")

  cat("   CUSUM for:", h1, "-", h2, "\n")

  spread <- gas[[paste0(h1, "_log")]] - gas[[paste0(h2, "_log")]]

  if (length(spread) < 50) next

  df_c <- data.frame(
    y = spread,
    trend = seq_along(spread)
  )

  cusum_test <- tryCatch({
    ef <- strucchange::efp(y ~ 1, data = df_c, type = "OLS-CUSUM")
    sc <- strucchange::sctest(ef)

    tibble(
      Test = "OLS-CUSUM",
      Statistic = as.numeric(sc$statistic),
      p_value = sc$p.value
    )
  }, error = function(e) {
    tibble(Test = "OLS-CUSUM", Statistic = NA_real_, p_value = NA_real_)
  })

  mosum_test <- tryCatch({
    ef <- strucchange::efp(y ~ 1, data = df_c, type = "OLS-MOSUM")
    sc <- strucchange::sctest(ef)

    tibble(
      Test = "OLS-MOSUM",
      Statistic = as.numeric(sc$statistic),
      p_value = sc$p.value
    )
  }, error = function(e) {
    tibble(Test = "OLS-MOSUM", Statistic = NA_real_, p_value = NA_real_)
  })

  cusum_spread_rows[[pair_name]] <- bind_rows(cusum_test, mosum_test) |>
    mutate(
      Pair = paste(h1, "-", h2),
      Hub_A = h1,
      Hub_B = h2,
      Specification = "intercept only",
      Reject_5pct = p_value < 0.05,
      .before = 1
    )
}

cusum_spreads <- if (length(cusum_spread_rows) > 0) {
  bind_rows(cusum_spread_rows)
} else {
  tibble()
}

write_csv(cusum_spreads, file.path(rob_dir, "CUSUM_spread_stability.csv"))



# Summary dashboard

summary_lines <- c(
  paste("Unit Root & Structural Break Summary —", Sys.time()),
  "",
  sprintf(
    "Sample: %s to %s (%d MA3-balanced observations)",
    min(gas$Date), max(gas$Date), nrow(gas)
  ),
  "",
  "Baseline Series: JKM, NBP, TTF, HH, Brent_MA3 (log price levels and differences)",
  "Gas-Gas Spreads: JKM-NBP, JKM-TTF, JKM-HH, NBP-TTF, NBP-HH, TTF-HH (unity coeff)",
  "",
  "=== THREE-WAY CONSENSUS CLASSIFICATION (Log Price Levels) ===",
  ""
)

if (nrow(consensus_table) > 0) {
  for (i in seq_len(nrow(consensus_table))) {
    r <- consensus_table[i, ]
    summary_lines <- c(
      summary_lines,
      sprintf(
        "  %-12s %s: %s",
        r$Series,
        r$Integration_Order,
        r$Classification
      ),
      sprintf(
        "         %s",
        r$Interpretation
      )
    )
  }
}

summary_lines <- c(
  summary_lines,
  "",
  "=== MARGINAL BAI PERRON BREAK COUNTS (BIC, intercept + trend) ===",
  ""
)

if (nrow(bp_prices_bic) > 0) {
  for (i in seq_len(nrow(bp_prices_bic))) {
    r <- bp_prices_bic[i, ]
    summary_lines <- c(
      summary_lines,
      sprintf("  %-12s %d breaks (BIC = %.2f)", r$Series, r$Breaks_BIC, r$BIC_value)
    )
  }
}

summary_lines <- c(
  summary_lines,
  "",
  "=== GAS-GAS SPREAD BAI PERRON BREAK COUNTS (BIC, intercept only) ===",
  ""
)

if (nrow(bp_spreads_bic) > 0) {
  for (i in seq_len(nrow(bp_spreads_bic))) {
    r <- bp_spreads_bic[i, ]
    summary_lines <- c(
      summary_lines,
      sprintf("  %-14s %d breaks (BIC = %.2f)", r$Pair, r$Breaks_BIC, r$BIC_value)
    )
  }
}

summary_lines <- c(
  summary_lines,
  "",
  "=== BREAK DATE ALIGNMENT (Spread vs Marginal) ===",
  ""
)

if (nrow(alignment_tbl) > 0) {
  for (i in seq_len(nrow(alignment_tbl))) {
    r <- alignment_tbl[i, ]
    summary_lines <- c(
      summary_lines,
      sprintf(
        "  %s break %d at %s: Hub_A (%s) nearest break %s days away, Hub_B (%s) nearest break %s days away",
        r$Pair,
        r$Spread_break_num,
        as.character(r$Spread_break_date),
        r$Hub_A,
        ifelse(is.na(r$Days_to_Hub_A_break), "none", as.character(r$Days_to_Hub_A_break)),
        r$Hub_B,
        ifelse(is.na(r$Days_to_Hub_B_break), "none", as.character(r$Days_to_Hub_B_break))
      )
    )
  }
} else {
  summary_lines <- c(summary_lines, "  No alignment data available.")
}

summary_lines <- c(
  summary_lines,
  "",
  "=== TECHNICAL NOTES ===",
  "  Log Price Levels: Unit root battery includes ADF (drift), Phillips-Perron, KPSS (level null),",
  "                    Zivot-Andrews (intercept model), and Zivot-Andrews (intercept+trend model).",
  "  Gas-Gas Spreads:   Unit root battery includes ADF (drift), Phillips-Perron, KPSS (level null),",
  "                     and Zivot-Andrews (intercept model only). Trend-break model omitted on spreads",
  "                     as deterministic trend breaks in differentials imply economically inconsistent",
  "                     long-run divergence.",
  "  Price Spec:        Bai-Perron, CUSUM, MOSUM on log levels use 'y ~ 1 + trend' specification.",
  "                     Descriptive interpretation: breaks are documented features of the series.",
  "  Spread Spec:       Bai-Perron, CUSUM, MOSUM on spreads use 'y ~ 1' specification (intercept only)",
  "                     reflecting constant-mean regime shifts in market integration.",
  "  Three-Way Space:   Space 1 (I(0)): baseline tests reject unit root",
  "                     Space 2 (TS with break): baseline fail to reject, break tests reject",
  "                     Space 3 (I(1)): no test rejects unit root null"
)

writeLines(
  summary_lines,
  file.path(bt_dir, "unit_root_break_summary.txt")
)

cat(paste(summary_lines, collapse = "\n"), "\n")



cat("\n== Unit Root & Structural Break tests finished ==\n")
cat("   Outputs in:\n")
cat("     ", ur_dir, "\n")
cat("     ", bp_dir, "\n")
cat("     ", sp_dir, "\n")
cat("     ", ba_dir, "\n")
cat("     ", rob_dir, "\n")