# Descriptive Analysis
# Input: gas_clean_balanced.csv
# Analysis: price levels, returns, correlations, spreads, crisis windows

# --- Packages ---
pkgs <- c("dplyr", "readr", "tidyr", "moments", "tseries", "FinTS",
          "tibble", "here")
to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

project_dir      <- here::here()
clean_input_file <- file.path(project_dir, "Model", "model output",
                              "Data Cleaning", "gas_clean_balanced.csv")
diag_model_dir   <- file.path(project_dir, "Model", "model output",
                              "Descriptive Analysis")
if (!dir.exists(diag_model_dir)) dir.create(diag_model_dir, recursive = TRUE)

# --- Load ---
if (!file.exists(clean_input_file)) stop("Input file not found: ", clean_input_file)

gas <- read_csv(clean_input_file, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

# --- Column definitions ---
gas_hubs <- c("JKM", "NBP", "TTF", "HH")
all_hubs <- c(gas_hubs, "Brent_crude")

# --- Validate balanced input ---
if (anyNA(gas[all_hubs])) {
  stop("Balanced input contains missing values. Re-run Data Cleaning.")
}
if (any(gas[all_hubs] <= 0, na.rm = TRUE)) {
  stop("Non-positive prices found. Log transformations are unsafe.")
}

cat(sprintf("Sample period: %s to %s (%d trading days)\n",
            min(gas$Date), max(gas$Date), nrow(gas)))

# --- Pair definitions ---
gas_pairs <- combn(gas_hubs, 2, simplify = FALSE)
pair_labels <- sapply(gas_pairs, paste, collapse = "-")

compute_stats <- function(df_long, value_col = "Value",
                          include_jb = TRUE, include_ann_sd = FALSE) {
  df_long %>%
    group_by(Hub) %>%
    summarise(
      N      = sum(!is.na(.data[[value_col]])),
      Mean   = mean(.data[[value_col]], na.rm = TRUE),
      SD     = sd(.data[[value_col]], na.rm = TRUE),
      Annualized_SD = if (include_ann_sd) {
        sd(.data[[value_col]], na.rm = TRUE) * sqrt(252)
      } else NA_real_,
      Min    = min(.data[[value_col]], na.rm = TRUE),
      P5     = quantile(.data[[value_col]], 0.05, na.rm = TRUE),
      Median = median(.data[[value_col]], na.rm = TRUE),
      P95    = quantile(.data[[value_col]], 0.95, na.rm = TRUE),
      Max    = max(.data[[value_col]], na.rm = TRUE),
      Skew   = moments::skewness(.data[[value_col]], na.rm = TRUE),
      Kurt   = moments::kurtosis(.data[[value_col]], na.rm = TRUE) - 3,
      JB_stat = if (include_jb) {
        x <- na.omit(.data[[value_col]])
        if (length(x) > 5) tseries::jarque.bera.test(x)$statistic else NA_real_
      } else NA_real_,
      JB_p   = if (include_jb) {
        x <- na.omit(.data[[value_col]])
        if (length(x) > 5) tseries::jarque.bera.test(x)$p.value else NA_real_
      } else NA_real_,
      .groups = "drop"
    ) %>%
    { if (!include_ann_sd) select(., -Annualized_SD) else . }
}

# Price levels
levels_long <- gas %>%
  pivot_longer(all_of(all_hubs), names_to = "Hub", values_to = "Value")

stats_levels <- compute_stats(levels_long)
write_csv(stats_levels, file.path(diag_model_dir, "stats_levels.csv"))

# Log price levels
log_levels_long <- levels_long %>%
  mutate(Value = log(Value))

stats_log_levels <- compute_stats(log_levels_long)
write_csv(stats_log_levels, file.path(diag_model_dir, "stats_log_levels.csv"))

log_levels_out <- levels_long %>%
  mutate(Log_Price = log(Value)) %>%
  select(Date, Hub, Log_Price)
write_csv(log_levels_out, file.path(diag_model_dir, "log_levels_long.csv"))

# Log returns and extreme returns
returns_wide <- gas %>%
  arrange(Date) %>%
  mutate(across(all_of(all_hubs),
                ~ log(.x / dplyr::lag(.x)),
                .names = "{.col}_ret")) %>%
  slice(-1)  # drop first row (all returns are NA)

# --- Verify equal return counts ---
ret_cols <- paste0(all_hubs, "_ret")
ret_counts <- colSums(!is.na(returns_wide[ret_cols]))
if (length(unique(ret_counts)) != 1) {
  stop("Unequal return counts across hubs: ", paste(ret_counts, collapse = ", "))
}
if (anyNA(returns_wide[ret_cols])) {
  stop("Missing returns remain after dropping the first lagged row.")
}
cat(sprintf("  Return sample: %d observations per series\n", ret_counts[1]))

# Long format for plotting
returns_long <- returns_wide %>%
  select(Date, all_of(ret_cols)) %>%
  pivot_longer(all_of(ret_cols), names_to = "Hub", values_to = "Value") %>%
  mutate(Hub = sub("_ret$", "", Hub))

write_csv(returns_long, file.path(diag_model_dir, "log_returns_long.csv"))

# Return statistics with annualized SD
stats_returns <- compute_stats(returns_long, include_ann_sd = TRUE)
write_csv(stats_returns, file.path(diag_model_dir, "stats_log_returns.csv"))

# --- Extreme returns: top 10 positive and negative per hub ---
extreme_returns <- returns_long %>%
  group_by(Hub) %>%
  arrange(desc(Value)) %>%
  mutate(Rank_pos = row_number()) %>%
  ungroup() %>%
  filter(Rank_pos <= 10) %>%
  mutate(Direction = "Positive") %>%
  select(Hub, Direction, Rank = Rank_pos, Date, Log_Return = Value) %>%
  bind_rows(
    returns_long %>%
      group_by(Hub) %>%
      arrange(Value) %>%
      mutate(Rank_neg = row_number()) %>%
      ungroup() %>%
      filter(Rank_neg <= 10) %>%
      mutate(Direction = "Negative") %>%
      select(Hub, Direction, Rank = Rank_neg, Date, Log_Return = Value)
  ) %>%
  arrange(Hub, Direction, Rank)

write_csv(extreme_returns, file.path(diag_model_dir, "extreme_returns.csv"))


## Correlation matrices
# Level correlations are descriptive only — I(1) prices will correlate
# spuriously. Return correlations capture genuine short-run co-movement.

cor_levels <- gas %>%
  select(all_of(all_hubs)) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("Hub")

write_csv(cor_levels, file.path(diag_model_dir, "correlation_levels.csv"))

# Returns: use the balanced return matrix (first row already dropped)
return_matrix <- returns_wide %>%
  select(all_of(ret_cols)) %>%
  rename_with(~ sub("_ret$", "", .x))

cor_returns <- cor(return_matrix, use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("Hub")

write_csv(cor_returns, file.path(diag_model_dir, "correlation_returns.csv"))


## Spread descriptives
# log(P_i) - log(P_j) for six undirected gas pairs.
# Market integration implies bounded, mean-reverting spreads.
# Threshold: |spread| > 0.5 corresponds to a ~65% price ratio gap (exp(0.5) - 1).

# Build spread long-format
spread_list <- lapply(seq_along(gas_pairs), function(k) {
  h1 <- gas_pairs[[k]][1]; h2 <- gas_pairs[[k]][2]
  tibble(
    Date   = gas$Date,
    Pair   = pair_labels[k],
    Spread = log(gas[[h1]]) - log(gas[[h2]])
  )
})
spread_long <- bind_rows(spread_list)
write_csv(spread_long, file.path(diag_model_dir, "spread_levels_long.csv"))

# Spread summary statistics
spread_summary <- spread_long %>%
  group_by(Pair) %>%
  summarise(
    N                = n(),
    Mean             = mean(Spread),
    SD               = sd(Spread),
    Min              = min(Spread),
    P5               = quantile(Spread, 0.05),
    Median           = median(Spread),
    P95              = quantile(Spread, 0.95),
    Max              = max(Spread),
    Skew             = moments::skewness(Spread),
    Kurt             = moments::kurtosis(Spread) - 3,
    Mean_Abs_Spread  = mean(abs(Spread)),
    Pct_above_0.5    = round(100 * mean(abs(Spread) > 0.5), 2),
    Pct_above_1.0    = round(100 * mean(abs(Spread) > 1.0), 2),
    .groups = "drop"
  )

write_csv(spread_summary, file.path(diag_model_dir, "spread_summary.csv"))


# Rolling return correlations
# 60-day window for short-run dynamics; 250-day for the slower structural trend.

compute_rolling_cor <- function(ret_wide, pairs, pair_labels, window) {
  n <- nrow(ret_wide)
  if (n < window) return(tibble())

  results <- list()
  for (k in seq_along(pairs)) {
    h1 <- pairs[[k]][1]; h2 <- pairs[[k]][2]
    x  <- ret_wide[[h1]]; y <- ret_wide[[h2]]
    cors <- rep(NA_real_, n)
    for (t in window:n) {
      idx <- (t - window + 1):t
      cors[t] <- cor(x[idx], y[idx], use = "complete.obs")
    }
    results[[k]] <- tibble(
      Date        = ret_wide$Date[window:n],
      Pair        = pair_labels[k],
      Correlation = cors[window:n]
    )
  }
  bind_rows(results)
}

# Build wide return frame with Date and hub return columns (no _ret suffix)
ret_wide_dated <- returns_wide %>%
  select(Date, all_of(ret_cols)) %>%
  rename_with(~ sub("_ret$", "", .x), .cols = all_of(ret_cols))

rolling_60  <- compute_rolling_cor(ret_wide_dated, gas_pairs, pair_labels, 60)
rolling_250 <- compute_rolling_cor(ret_wide_dated, gas_pairs, pair_labels, 250)

write_csv(rolling_60,  file.path(diag_model_dir, "rolling_correlation_60d.csv"))
write_csv(rolling_250, file.path(diag_model_dir, "rolling_correlation_250d.csv"))


## Crisis-window descriptives
# Mutually exclusive event regimes. The 2021-2022 energy crisis is split at
# the Feb 2022 invasion to avoid double-counting the two distinct shocks.

stress_windows <- tibble(
  Window = c("Pre-COVID",
             "COVID shock",
             "Interim recovery",
             "Energy crisis (pre-war)",
             "Post-invasion crisis",
             "Normalization"),
  Start  = as.Date(c(NA, "2020-02-01", "2020-07-01",
                      "2021-09-01", "2022-02-24", "2023-01-01")),
  End    = as.Date(c("2020-01-31", "2020-06-30", "2021-08-31",
                      "2022-02-23", "2022-12-31", NA))
)
# Dynamic boundaries: first window starts at sample start, last ends at sample end
stress_windows$Start[1] <- min(gas$Date)
stress_windows$End[nrow(stress_windows)] <- max(gas$Date)

# --- 7a. Return statistics by window ---
ret_by_window <- list()
for (i in seq_len(nrow(stress_windows))) {
  w <- stress_windows[i, ]
  sub <- returns_long %>%
    filter(Date >= w$Start, Date <= w$End)
  if (nrow(sub) == 0) next
  stats <- sub %>%
    group_by(Hub) %>%
    summarise(
      N              = n(),
      Mean_Return    = mean(Value),
      SD             = sd(Value),
      Annualized_SD  = sd(Value) * sqrt(252),
      Skew           = moments::skewness(Value),
      Kurt           = moments::kurtosis(Value) - 3,
      .groups = "drop"
    ) %>%
    mutate(Window = w$Window, .before = 1)
  ret_by_window[[i]] <- stats
}
write_csv(bind_rows(ret_by_window),
          file.path(diag_model_dir, "stats_returns_by_window.csv"))

# --- 7b. Spread statistics by window ---
spread_by_window <- list()
for (i in seq_len(nrow(stress_windows))) {
  w <- stress_windows[i, ]
  sub <- spread_long %>%
    filter(Date >= w$Start, Date <= w$End)
  if (nrow(sub) == 0) next
  stats <- sub %>%
    group_by(Pair) %>%
    summarise(
      N               = n(),
      Mean_Spread     = mean(Spread),
      SD_Spread       = sd(Spread),
      Mean_Abs_Spread = mean(abs(Spread)),
      .groups = "drop"
    ) %>%
    mutate(Window = w$Window, .before = 1)
  spread_by_window[[i]] <- stats
}
write_csv(bind_rows(spread_by_window),
          file.path(diag_model_dir, "stats_spreads_by_window.csv"))

# --- 7c. Gas-gas return correlations by window ---
# Reports gas-hub pairwise correlations only (excludes Brent).
cor_by_window <- list()
for (i in seq_len(nrow(stress_windows))) {
  w <- stress_windows[i, ]
  sub <- ret_wide_dated %>%
    filter(Date >= w$Start, Date <= w$End) %>%
    select(all_of(gas_hubs))
  if (nrow(sub) < 10) next
  cm <- cor(sub, use = "complete.obs") %>%
    as.data.frame() %>%
    rownames_to_column("Hub") %>%
    pivot_longer(-Hub, names_to = "Hub2", values_to = "Correlation") %>%
    filter(Hub < Hub2) %>%  # upper triangle only
    mutate(Window = w$Window, .before = 1)
  cor_by_window[[i]] <- cm
}
write_csv(bind_rows(cor_by_window),
          file.path(diag_model_dir, "correlation_returns_by_window.csv"))


# Annual and monthly volatility

# --- 8a. Annualized volatility by year ---
vol_by_year <- returns_long %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>%
  group_by(Hub, Year) %>%
  summarise(
    N             = n(),
    SD            = sd(Value),
    Annualized_SD = sd(Value) * sqrt(252),
    .groups = "drop"
  ) %>%
  arrange(Hub, Year)

write_csv(vol_by_year, file.path(diag_model_dir, "volatility_by_year.csv"))

# --- 8b. Return statistics by calendar month ---
stats_by_month <- returns_long %>%
  mutate(Month = as.integer(format(Date, "%m"))) %>%
  group_by(Hub, Month) %>%
  summarise(
    N          = n(),
    Mean       = mean(Value),
    SD         = sd(Value),
    .groups = "drop"
  ) %>%
  mutate(Month_Name = month.abb[Month]) %>%
  arrange(Hub, Month)

write_csv(stats_by_month, file.path(diag_model_dir, "stats_returns_by_month.csv"))


# Serial correlation — Ljung-Box on returns
# Significant values indicate autocorrelation in daily price changes,
# motivating the AR(2) component in the Kalman filter specification.

lb_results <- returns_long %>%
  group_by(Hub) %>%
  summarise(
    LB_Q10 = Box.test(Value, lag = 10, type = "Ljung-Box")$statistic,
    LB_p10 = Box.test(Value, lag = 10, type = "Ljung-Box")$p.value,
    LB_Q20 = Box.test(Value, lag = 20, type = "Ljung-Box")$statistic,
    LB_p20 = Box.test(Value, lag = 20, type = "Ljung-Box")$p.value,
    .groups = "drop"
  )

write_csv(lb_results, file.path(diag_model_dir, "ljung_box_returns.csv"))


# ARCH effects — Engle (1982) LM test
# Expected in daily energy returns. Constant measurement variance is retained
# for parsimony; quasi-MLE robustness handles residual heteroskedasticity.

arch_results <- returns_long %>%
  group_by(Hub) %>%
  summarise(
    ARCH_LM_stat = FinTS::ArchTest(Value, lags = 10)$statistic,
    ARCH_LM_p    = FinTS::ArchTest(Value, lags = 10)$p.value,
    .groups = "drop"
  )

write_csv(arch_results, file.path(diag_model_dir, "arch_lm_returns.csv"))


cat("\n== Finished Descriptive Analysis ==\n")
cat(sprintf("   %d output files written to %s\n", 19, diag_model_dir))