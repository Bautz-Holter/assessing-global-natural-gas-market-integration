# Unit Root and Structural Break Visualisation
# Input: Unit root tests, Bai-Perron breaks, break alignment diagnostics
# Output: Publication-ready plots and tables; no embedded titles; LaTeX-compatible

pkgs <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr",
  "gt",
  "here",
  "tibble",
  "scales",
  "webshot2"
)

to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

project_dir <- here::here()

clean_dir <- file.path(
  project_dir,
  "Model", "model output", "Data Cleaning"
)

bt_dir <- file.path(
  project_dir,
  "Model", "model output", "Unit Root and Structural Break"
)

ur_dir <- file.path(bt_dir, "Unit_Root")
bp_dir <- file.path(bt_dir, "Bai_Perron")
sp_dir <- file.path(bt_dir, "Pairwise_Spreads")
ba_dir <- file.path(bt_dir, "Break_Alignment")
rob_dir <- file.path(bt_dir, "Robustness")

out_root <- file.path(
  project_dir,
  "Visualization", "Plot outputs", "Unit Root and Structural Break"
)

plots_dir <- file.path(out_root, "Plots")
tables_dir <- file.path(out_root, "Tables")

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# Required files

read_csv_safe <- function(path) {
  out <- tryCatch(
    read_csv(path, show_col_types = FALSE),
    error = function(e) tibble()
  )
  out
}

required_files <- c(
  file.path(clean_dir, "gas_clean_balanced.csv"),
  file.path(ur_dir, "unit_root_prices.csv"),
  file.path(ur_dir, "unit_root_spreads.csv"),
  file.path(ur_dir, "three_way_consensus_classification.csv"),
  file.path(bp_dir, "BaiPerron_BIC_selection.csv"),
  file.path(bp_dir, "BaiPerron_regime_summary.csv"),
  file.path(sp_dir, "spread_BaiPerron_BIC.csv"),
  file.path(rob_dir, "CUSUM_stability.csv"),
  file.path(rob_dir, "CUSUM_spread_stability.csv")
)

optional_files <- c(
  file.path(bp_dir, "BaiPerron_break_dates.csv"),
  file.path(sp_dir, "spread_BaiPerron_breaks.csv"),
  file.path(ba_dir, "break_date_alignment.csv")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required model output files:\n",
    paste(missing_files, collapse = "\n")
  )
}

# Load data

gas <- read_csv(file.path(clean_dir, "gas_clean_balanced.csv"), show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

all_hubs <- c("JKM", "NBP", "TTF", "HH", "Brent_MA3")
gas_hubs <- c("JKM", "NBP", "TTF", "HH")

ur_prices <- read_csv(file.path(ur_dir, "unit_root_prices.csv"), show_col_types = FALSE)

ur_spreads <- read_csv(file.path(ur_dir, "unit_root_spreads.csv"), show_col_types = FALSE)

classification <- read_csv(file.path(ur_dir, "three_way_consensus_classification.csv"), show_col_types = FALSE)

bp_bic <- read_csv(file.path(bp_dir, "BaiPerron_BIC_selection.csv"), show_col_types = FALSE)

regime_summary <- read_csv(file.path(bp_dir, "BaiPerron_regime_summary.csv"), show_col_types = FALSE) |>
  mutate(
    Start = as.Date(Start),
    End = as.Date(End)
  )

bp_dates <- read_csv_safe(file.path(bp_dir, "BaiPerron_break_dates.csv")) |>
  mutate(
    Break_date = as.Date(Break_date),
    across(any_of(c("CI_lower", "CI_upper")), as.Date),
    Hub = Series
  )

regime_means <- if (nrow(regime_summary) > 0) {
  gas |>
    mutate(across(all_of(all_hubs), log, .names = "{.col}_log")) |>
    select(Date, ends_with("_log")) |>
    pivot_longer(-Date, names_to = "Hub", values_to = "Log_Price") |>
    mutate(Hub = sub("_log$", "", Hub)) |>
    left_join(
      regime_summary |> select(Series, Start, End, Regime_Mean = Mean_log) |>
        rename(Hub = Series),
      by = "Hub",
      relationship = "many-to-many"
    ) |>
    filter(Date >= Start & Date <= End) |>
    select(Date, Hub, Log_Price, Regime_Mean)
} else {
  tibble()
}

spread_bic <- read_csv(file.path(sp_dir, "spread_BaiPerron_BIC.csv"), show_col_types = FALSE)

spread_bp <- read_csv_safe(file.path(sp_dir, "spread_BaiPerron_breaks.csv")) |>
  mutate(
    Break_date = as.Date(Break_date),
    across(any_of(c("CI_lower", "CI_upper")), as.Date)
  )

alignment <- read_csv_safe(file.path(ba_dir, "break_date_alignment.csv")) |>
  mutate(
    Spread_break_date = as.Date(Spread_break_date),
    across(any_of(c("Nearest_Hub_A_break", "Nearest_Hub_B_break")), as.Date)
  )

cusum <- read_csv(file.path(rob_dir, "CUSUM_stability.csv"), show_col_types = FALSE)

cusum_spread <- read_csv(file.path(rob_dir, "CUSUM_spread_stability.csv"), show_col_types = FALSE)

# Definitions

hub_colors <- c(
  "JKM" = "#1B1B3A",
  "TTF" = "#1F78B4",
  "NBP" = "#33A02C",
  "HH" = "#FF7F00",
  "Brent_MA3" = "#B22182"
)

hub_labels <- c(
  "JKM" = "JKM",
  "TTF" = "TTF",
  "NBP" = "NBP",
  "HH" = "Henry Hub",
  "Brent_MA3" = "Brent MA3"
)

label_hub <- function(x) {
  x <- as.character(x)
  out <- ifelse(x %in% names(hub_labels), hub_labels[x], x)
  unname(out)
}

theme_thesis <- function() {
  theme_bw(base_family = "serif", base_size = 10) +
    theme(
      text = element_text(color = "black"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
      strip.text = element_text(size = 10),
      legend.position = "none",
      plot.margin = margin(6, 10, 6, 6)
    )
}

format_table <- function(x) {
  x |>
    tab_options(
      table.font.names = "Times New Roman",
      table.font.size = 11,
      table.border.top.style = "solid",
      table.border.top.width = px(2),
      table.border.top.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      table.border.bottom.color = "black",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      column_labels.font.weight = "bold",
      table_body.hlines.width = px(0),
      table_body.vlines.width = px(0),
      data_row.padding = px(4),
      source_notes.font.size = 9
    ) |>
    opt_table_font(font = list("Times New Roman", "Times", "serif"))
}

save_gt_png <- function(gt_object, filename) {
  gt::gtsave(gt_object, filename)
}

add_stars_from_pval <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  )
}

add_stars_from_reject <- function(reject) {
  case_when(
    is.na(reject) ~ "",
    reject == TRUE ~ "*",
    TRUE ~ ""
  )
}

sample_note <- paste0(
  "Sample: daily balanced observations, ",
  format(min(gas$Date), "%d %b %Y"),
  " to ",
  format(max(gas$Date), "%d %b %Y"),
  "."
)

stars_note <- "***, **, * denote rejection of the null at the 1%, 5%, and 10% levels, respectively."

# Prepare log levels and gas-gas spreads for plots

gas_log <- gas |>
  mutate(across(all_of(all_hubs), log, .names = "{.col}_log"))

gas_log_long <- gas_log |>
  select(Date, ends_with("_log")) |>
  pivot_longer(-Date, names_to = "Hub", values_to = "Log_Price") |>
  mutate(
    Hub = sub("_log$", "", Hub),
    Hub = factor(Hub, levels = all_hubs)
  )

spread_long_list <- list()

for (i in seq_along(gas_hubs)) {
  for (j in seq_along(gas_hubs)) {
    if (i >= j) next

    h1 <- gas_hubs[i]
    h2 <- gas_hubs[j]
    pair <- paste(h1, "-", h2)

    spread_long_list[[pair]] <- tibble(
      Date = gas_log$Date,
      Pair = pair,
      Hub_A = h1,
      Hub_B = h2,
      Spread = gas_log[[paste0(h1, "_log")]] - gas_log[[paste0(h2, "_log")]]
    )
  }
}

spread_long <- bind_rows(spread_long_list)

# 1. Unit root tables: Price Levels Summary

ur_prices_table <- ur_prices |>
  mutate(
    Series = Series,
    ADF_Stat = paste0(
      format(round(ADF_stat, 3), nsmall = 3),
      add_stars_from_reject(ADF_reject_5pct)
    ),
    ADF_Result = case_when(
      ADF_reject_5pct == TRUE ~ "Stationary",
      ADF_reject_5pct == FALSE ~ "Unit root",
      TRUE ~ "NA"
    ),
    PP_Stat = paste0(
      format(round(PP_stat, 3), nsmall = 3),
      add_stars_from_pval(PP_p)
    ),
    PP_Result = case_when(
      PP_reject_5pct == TRUE ~ "Stationary",
      PP_reject_5pct == FALSE ~ "Unit root",
      TRUE ~ "NA"
    ),
    KPSS_Stat = paste0(
      format(round(KPSS_stat, 3), nsmall = 3),
      add_stars_from_pval(KPSS_p)
    ),
    KPSS_Result = case_when(
      KPSS_reject_5pct == TRUE ~ "Unit root",
      KPSS_reject_5pct == FALSE ~ "Stationary",
      TRUE ~ "NA"
    ),
    ZA_Int_Stat = paste0(
      format(round(ZA_intercept_stat, 3), nsmall = 3),
      add_stars_from_reject(ZA_intercept_reject_5pct)
    ),
    ZA_Int_Result = case_when(
      ZA_intercept_reject_5pct == TRUE ~ "Stationary",
      ZA_intercept_reject_5pct == FALSE ~ "Unit root",
      TRUE ~ "NA"
    ),
    ZA_Trend_Stat = paste0(
      format(round(ZA_trend_stat, 3), nsmall = 3),
      add_stars_from_reject(ZA_trend_reject_5pct)
    ),
    ZA_Trend_Result = case_when(
      ZA_trend_reject_5pct == TRUE ~ "Stationary",
      ZA_trend_reject_5pct == FALSE ~ "Unit root",
      TRUE ~ "NA"
    )
  ) |>
  select(
    Series,
    ADF_Stat, ADF_Result,
    PP_Stat, PP_Result,
    KPSS_Stat, KPSS_Result,
    ZA_Int_Stat, ZA_Int_Result,
    ZA_Trend_Stat, ZA_Trend_Result
  ) |>
  gt() |>
  cols_label(
    Series = "Series",
    ADF_Stat = "Stat.", ADF_Result = "Result",
    PP_Stat = "Stat.", PP_Result = "Result",
    KPSS_Stat = "Stat.", KPSS_Result = "Result",
    ZA_Int_Stat = "Stat.", ZA_Int_Result = "Result",
    ZA_Trend_Stat = "Stat.", ZA_Trend_Result = "Result"
  ) |>
  tab_spanner(label = "ADF", columns = c(ADF_Stat, ADF_Result)) |>
  tab_spanner(label = "Phillips-Perron", columns = c(PP_Stat, PP_Result)) |>
  tab_spanner(label = "KPSS", columns = c(KPSS_Stat, KPSS_Result)) |>
  tab_spanner(label = "ZA Intercept", columns = c(ZA_Int_Stat, ZA_Int_Result)) |>
  tab_spanner(label = "ZA Intercept+Trend", columns = c(ZA_Trend_Stat, ZA_Trend_Result)) |>
  cols_align(align = "left", columns = c(Series, ends_with("Result"))) |>
  cols_align(align = "right", columns = ends_with("Stat")) |>
  tab_source_note(
    "ADF, PP, and ZA test the null of a unit root; KPSS tests the null of stationarity. *, **, *** indicate significance at 5%, 1%, 0.1% levels for tests with p-values; * indicates 5% significance for tests without p-values."
  ) |>
  tab_source_note(sample_note) |>
  format_table()

save_gt_png(ur_prices_table, file.path(tables_dir, "Unit_root_prices.png"))

# 1b. Unit root tables: Gas-Gas Spreads Summary

ur_spreads_table <- ur_spreads |>
  mutate(
    Pair_Label = Pair,
    ADF_Stat = paste0(
      format(round(ADF_stat, 3), nsmall = 3),
      add_stars_from_reject(ADF_reject_5pct)
    ),
    ADF_Result = case_when(
      ADF_reject_5pct == TRUE ~ "Stationary",
      ADF_reject_5pct == FALSE ~ "Unit root",
      TRUE ~ "NA"
    ),
    PP_Stat = paste0(
      format(round(PP_stat, 3), nsmall = 3),
      add_stars_from_pval(PP_p)
    ),
    PP_Result = case_when(
      PP_reject_5pct == TRUE ~ "Stationary",
      PP_reject_5pct == FALSE ~ "Unit root",
      TRUE ~ "NA"
    ),
    KPSS_Stat = paste0(
      format(round(KPSS_stat, 3), nsmall = 3),
      add_stars_from_pval(KPSS_p)
    ),
    KPSS_Result = case_when(
      KPSS_reject_5pct == TRUE ~ "Unit root",
      KPSS_reject_5pct == FALSE ~ "Stationary",
      TRUE ~ "NA"
    ),
    ZA_Stat = paste0(
      format(round(ZA_intercept_stat, 3), nsmall = 3),
      add_stars_from_reject(ZA_intercept_reject_5pct)
    ),
    ZA_Result = case_when(
      ZA_intercept_reject_5pct == TRUE ~ "Stationary",
      ZA_intercept_reject_5pct == FALSE ~ "Unit root",
      TRUE ~ "NA"
    )
  ) |>
  select(
    Pair_Label,
    ADF_Stat, ADF_Result,
    PP_Stat, PP_Result,
    KPSS_Stat, KPSS_Result,
    ZA_Stat, ZA_Result
  ) |>
  gt() |>
  cols_label(
    Pair_Label = "Spread",
    ADF_Stat = "Stat.", ADF_Result = "Result",
    PP_Stat = "Stat.", PP_Result = "Result",
    KPSS_Stat = "Stat.", KPSS_Result = "Result",
    ZA_Stat = "Stat.", ZA_Result = "Result"
  ) |>
  tab_spanner(label = "ADF", columns = c(ADF_Stat, ADF_Result)) |>
  tab_spanner(label = "Phillips-Perron", columns = c(PP_Stat, PP_Result)) |>
  tab_spanner(label = "KPSS", columns = c(KPSS_Stat, KPSS_Result)) |>
  tab_spanner(label = "ZA Intercept", columns = c(ZA_Stat, ZA_Result)) |>
  cols_align(align = "left", columns = c(Pair_Label, ends_with("Result"))) |>
  cols_align(align = "right", columns = ends_with("Stat")) |>
  tab_source_note(
    "Gas-gas spreads are unity-coefficient log ratios. ADF, PP, and ZA test the null of a unit root; KPSS tests the null of stationarity. ZA intercept model omits trend to avoid economically inconsistent divergence implications."
  ) |>
  tab_source_note(sample_note) |>
  format_table()

save_gt_png(ur_spreads_table, file.path(tables_dir, "Unit_root_spreads.png"))

# 2. Bai-Perron marginal break tables

bp_bic_table <- bp_bic |>
  select(Series, Breaks_BIC, BIC_value, Specification) |>
  rename(Hub = Series, BP_specification = Specification) |>
  gt() |>
  cols_label(
    Hub = "Series",
    Breaks_BIC = "Breaks",
    BIC_value = "BIC",
    BP_specification = "Specification"
  ) |>
  fmt_number(columns = BIC_value, decimals = 2) |>
  fmt_number(columns = Breaks_BIC, decimals = 0) |>
  cols_align(align = "left", columns = c(Hub, BP_specification)) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_source_note(
    "Bai-Perron applied to marginal log levels using an intercept and trend specification."
  ) |>
  format_table()

save_gt_png(bp_bic_table, file.path(tables_dir, "BaiPerron_BIC.png"))

if (nrow(bp_dates) > 0) {
  bp_dates_table <- bp_dates |>
    select(any_of(c(
      "Hub", "Break_num", "Break_date", "CI_lower",
      "CI_upper", "CI_width_days", "Specification"
    ))) |>
    rename(BP_specification = Specification) |>
    gt() |>
    cols_label(
      Hub = "Series",
      Break_num = "#",
      Break_date = "Break date",
      CI_lower = "CI lower",
      CI_upper = "CI upper",
      CI_width_days = "CI width",
      BP_specification = "Specification"
    ) |>
    fmt_date(columns = any_of(c("Break_date", "CI_lower", "CI_upper")), date_style = "iso") |>
    fmt_number(columns = any_of(c("Break_num", "CI_width_days")), decimals = 0) |>
    cols_align(align = "left", columns = any_of(c("Hub", "BP_specification"))) |>
    cols_align(align = "right", columns = where(is.numeric)) |>
    tab_source_note(
      "Break dates are candidate deterministic shifts in marginal log price levels."
    ) |>
    format_table()

  save_gt_png(bp_dates_table, file.path(tables_dir, "BaiPerron_break_dates.png"))
}

regime_table <- regime_summary |>
  select(Series, Regime, Start, End, N_obs, Mean_log, SD_log, Mean_price, Shift_log, Shift_pct) |>
  rename(Hub = Series) |>
  gt(groupname_col = "Hub") |>
  cols_label(
    Regime = "Regime",
    Start = "Start",
    End = "End",
    N_obs = "N",
    Mean_log = "Mean log",
    SD_log = "S.D. log",
    Mean_price = "Mean price",
    Shift_log = "Shift log",
    Shift_pct = "Shift %"
  ) |>
  fmt_date(columns = c(Start, End), date_style = "iso") |>
  fmt_number(columns = N_obs, decimals = 0, use_seps = TRUE) |>
  fmt_number(columns = c(Mean_log, SD_log, Shift_log), decimals = 3) |>
  fmt_number(columns = Mean_price, decimals = 2) |>
  fmt_number(columns = Shift_pct, decimals = 1) |>
  sub_missing(missing_text = "—") |>
  tab_source_note(
    "Regime assignment follows the rule that the break date belongs to the new regime."
  ) |>
  format_table()

save_gt_png(regime_table, file.path(tables_dir, "BaiPerron_regime_summary.png"))

# 3. Bai-Perron marginal break plots

for (h in unique(as.character(regime_means$Hub))) {
  df <- regime_means |>
    filter(Hub == h)

  breaks_h <- if (nrow(bp_dates) > 0) {
    bp_dates |>
      filter(Hub == h) |>
      pull(Break_date)
  } else {
    as.Date(character())
  }

  breaks_h <- breaks_h[breaks_h > min(df$Date) & breaks_h < max(df$Date)]

  p <- df |>
    ggplot(aes(Date)) +
    geom_line(aes(y = Log_Price), color = "grey65", linewidth = 0.3, alpha = 0.65) +
    geom_line(aes(y = Regime_Mean), color = "#B2182B", linewidth = 0.9) +
    geom_vline(
      xintercept = breaks_h,
      linetype = "dashed",
      color = "grey35",
      linewidth = 0.45
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(x = NULL, y = "Log price") +
    theme_thesis()

  ggsave(
    file.path(plots_dir, paste0("BaiPerron_", h, ".png")),
    p,
    width = 10,
    height = 4.8,
    dpi = 300
  )
}

p_bp_overlay <- regime_means |>
  mutate(Hub = factor(Hub, levels = all_hubs)) |>
  group_by(Hub) |>
  mutate(Log_Price_Norm = Log_Price - first(Log_Price)) |>
  ungroup() |>
  ggplot(aes(Date, Log_Price_Norm, color = Hub)) +
  geom_line(linewidth = 0.4, alpha = 0.75) +
  geom_vline(
    data = bp_dates,
    aes(xintercept = Break_date, color = Hub),
    linetype = "dashed",
    linewidth = 0.35,
    alpha = 0.55
  ) +
  scale_color_manual(values = hub_colors, labels = hub_labels) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Normalised log price", color = NULL) +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm")
  )

ggsave(
  file.path(plots_dir, "BaiPerron_combined_overlay.png"),
  p_bp_overlay,
  width = 11,
  height = 5.5,
  dpi = 300
)

# 4. Spread Bai-Perron tables

spread_bic_table <- spread_bic |>
  select(Pair, Breaks_BIC, BIC_value, Specification) |>
  gt() |>
  cols_label(
    Pair = "Pair",
    Breaks_BIC = "Breaks",
    BIC_value = "BIC",
    Specification = "Specification"
  ) |>
  fmt_number(columns = BIC_value, decimals = 2) |>
  fmt_number(columns = Breaks_BIC, decimals = 0) |>
  cols_align(align = "left", columns = c(Pair, Specification)) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_source_note(
    "Bai-Perron applied to gas-gas log spreads using an intercept-only specification."
  ) |>
  format_table()

save_gt_png(spread_bic_table, file.path(tables_dir, "Spread_BaiPerron_BIC.png"))

if (nrow(spread_bp) > 0) {
  spread_bp_table <- spread_bp |>
    select(any_of(c(
      "Pair", "Break_num", "Break_date",
      "CI_lower", "CI_upper", "CI_width_days",
      "Specification"
    ))) |>
    gt() |>
    cols_label(
      Pair = "Pair",
      Break_num = "#",
      Break_date = "Break date",
      CI_lower = "CI lower",
      CI_upper = "CI upper",
      CI_width_days = "CI width",
      Specification = "Specification"
    ) |>
    fmt_date(columns = any_of(c("Break_date", "CI_lower", "CI_upper")), date_style = "iso") |>
    fmt_number(columns = any_of(c("Break_num", "CI_width_days")), decimals = 0) |>
    cols_align(align = "left", columns = any_of(c("Pair", "Specification"))) |>
    cols_align(align = "right", columns = where(is.numeric)) |>
    tab_source_note(
      "Spread break dates are candidate alpha regime dates in pairwise analysis."
    ) |>
    format_table()

  save_gt_png(spread_bp_table, file.path(tables_dir, "Spread_BaiPerron_break_dates.png"))
}

if (nrow(alignment) > 0) {
  alignment_table <- alignment |>
    select(
      Pair,
      Spread_break_num,
      Spread_break_date,
      Nearest_Hub_A_break,
      Days_to_Hub_A_break,
      Nearest_Hub_B_break,
      Days_to_Hub_B_break
    ) |>
    gt() |>
    cols_label(
      Pair = "Pair",
      Spread_break_num = "#",
      Spread_break_date = "Spread break",
      Nearest_Hub_A_break = "Nearest hub A",
      Days_to_Hub_A_break = "Days A",
      Nearest_Hub_B_break = "Nearest hub B",
      Days_to_Hub_B_break = "Days B"
    ) |>
    fmt_date(columns = c(Spread_break_date, Nearest_Hub_A_break, Nearest_Hub_B_break), date_style = "iso") |>
    fmt_number(columns = c(Spread_break_num, Days_to_Hub_A_break, Days_to_Hub_B_break), decimals = 0) |>
    cols_align(align = "left", columns = Pair) |>
    cols_align(align = "right", columns = where(is.numeric)) |>
    tab_source_note(
      "Day differences are measured relative to the spread break date."
    ) |>
    format_table()

  save_gt_png(alignment_table, file.path(tables_dir, "Break_date_alignment.png"))
}

# 5. Spread plots

p_spreads <- spread_long |>
  ggplot(aes(Date, Spread)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_line(color = "#1B1B3A", linewidth = 0.35) +
  facet_wrap(~Pair, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Log spread") +
  theme_thesis()

ggsave(
  file.path(plots_dir, "Spread_gas_gas.png"),
  p_spreads,
  width = 10,
  height = 7,
  dpi = 300
)

if (nrow(spread_bp) > 0) {
  for (pair_i in unique(spread_long$Pair)) {
    df <- spread_long |>
      filter(Pair == pair_i)

    h1 <- unique(df$Hub_A)
    h2 <- unique(df$Hub_B)

    pair_breaks <- spread_bp |>
      filter(Pair == pair_i) |>
      transmute(Break_date, Source = "Spread")

    h1_breaks <- bp_dates |>
      filter(Hub == h1) |>
      transmute(Break_date, Source = h1)

    h2_breaks <- bp_dates |>
      filter(Hub == h2) |>
      transmute(Break_date, Source = h2)

    vlines <- bind_rows(pair_breaks, h1_breaks, h2_breaks) |>
      filter(!is.na(Break_date))

    line_colors <- c(
      "Spread" = "#B2182B",
      h1 = hub_colors[h1],
      h2 = hub_colors[h2]
    )

    p <- df |>
      ggplot(aes(Date, Spread)) +
      geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
      geom_line(color = "grey35", linewidth = 0.35) +
      geom_vline(
        data = vlines,
        aes(xintercept = Break_date, color = Source),
        linetype = "dashed",
        linewidth = 0.55,
        alpha = 0.75
      ) +
      scale_color_manual(values = line_colors) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
      labs(x = NULL, y = "Log spread", color = NULL) +
      theme_thesis() +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(1.1, "cm")
      )

    out_name <- paste0("Spread_BP_overlay_", gsub(" - ", "_", pair_i), ".png")

    ggsave(
      file.path(plots_dir, out_name),
      p,
      width = 10,
      height = 4.8,
      dpi = 300
    )
  }
}

# 6. Robustness tables

cusum_table <- cusum |>
  rename(Hub = Series) |>
  mutate(
    Sig = add_stars_from_pval(p_value)
  ) |>
  select(Hub, Test, Statistic, p_value, Sig, Reject_5pct) |>
  gt() |>
  cols_label(
    Hub = "Series",
    Test = "Test",
    Statistic = "Statistic",
    p_value = "p",
    Sig = "",
    Reject_5pct = "Reject"
  ) |>
  fmt_number(columns = c(Statistic, p_value), decimals = 4) |>
  cols_align(align = "left", columns = c(Hub, Test, Sig)) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_source_note(
    "CUSUM and MOSUM test parameter stability in marginal log levels."
  ) |>
  tab_source_note(stars_note) |>
  format_table()

save_gt_png(cusum_table, file.path(tables_dir, "CUSUM_stability.png"))

cusum_spread_table <- cusum_spread |>
  mutate(Sig = add_stars_from_pval(p_value)) |>
  select(Pair, Test, Statistic, p_value, Sig, Reject_5pct) |>
  gt() |>
  cols_label(
    Pair = "Pair",
    Test = "Test",
    Statistic = "Statistic",
    p_value = "p",
    Sig = "",
    Reject_5pct = "Reject"
  ) |>
  fmt_number(columns = c(Statistic, p_value), decimals = 4) |>
  cols_align(align = "left", columns = c(Pair, Test, Sig)) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_source_note(
    "CUSUM and MOSUM test parameter stability in gas-gas log spreads."
  ) |>
  tab_source_note(stars_note) |>
  format_table()

save_gt_png(cusum_spread_table, file.path(tables_dir, "CUSUM_spread_stability.png"))

# Final check

expected_tables <- file.path(
  tables_dir,
  c(
    "Unit_root_prices.png",
    "Unit_root_spreads.png",
    "BaiPerron_BIC.png",
    "BaiPerron_regime_summary.png",
    "Spread_BaiPerron_BIC.png",
    "CUSUM_stability.png",
    "CUSUM_spread_stability.png"
  )
)

if (nrow(bp_dates) > 0) {
  expected_tables <- c(
    expected_tables,
    file.path(tables_dir, "BaiPerron_break_dates.png")
  )
}

if (nrow(spread_bp) > 0) {
  expected_tables <- c(
    expected_tables,
    file.path(tables_dir, "Spread_BaiPerron_break_dates.png")
  )
}

if (nrow(alignment) > 0) {
  expected_tables <- c(
    expected_tables,
    file.path(tables_dir, "Break_date_alignment.png")
  )
}

expected_plots <- file.path(
  plots_dir,
  c(
    "BaiPerron_combined_overlay.png",
    "Spread_gas_gas.png",
    paste0("BaiPerron_", unique(as.character(regime_means$Hub)), ".png")
  )
)

if (nrow(spread_bp) > 0) {
  expected_plots <- c(
    expected_plots,
    file.path(
      plots_dir,
      paste0("Spread_BP_overlay_", gsub(" - ", "_", unique(spread_long$Pair)), ".png")
    )
  )
}

expected_outputs <- c(expected_tables, expected_plots)
missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

cat("\n== Unit Root and Structural Break Visualisation Finished ==\n")
cat("   Plot directory:", plots_dir, "\n")
cat("   Table directory:", tables_dir, "\n")
cat("   PNG outputs created:", sum(file.exists(expected_outputs)), "of", length(expected_outputs), "\n")

if (length(missing_outputs) > 0) {
  cat("   PNG outputs not created:\n")
  cat(paste0("     ", basename(missing_outputs), collapse = "\n"), "\n")
}
