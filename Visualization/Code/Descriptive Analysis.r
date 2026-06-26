# Descriptive Analysis Visualisation
# Input: Model output CSVs from Descriptive Analysis, cleaned gas data
# Output: Publication-ready plots and tables with no embedded titles; LaTeX-compatible dimensions

pkgs <- c(
  "dplyr", "tidyr", "ggplot2", "readr", "gt", "here",
  "tibble", "scales", "webshot2", "grid"
)

to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

project_dir <- here::here()

model_dir <- file.path(project_dir, "Model", "model output", "Descriptive Analysis")
clean_dir <- file.path(project_dir, "Model", "model output", "Data Cleaning")

out_root   <- file.path(project_dir, "Visualization", "Plot outputs", "Descriptive Analysis")
plots_dir  <- file.path(out_root, "Plots")
tables_dir <- file.path(out_root, "Tables")

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# Required files
required_model_files <- c(
  "stats_levels.csv",
  "stats_log_levels.csv",
  "stats_log_returns.csv",
  "log_levels_long.csv",
  "log_returns_long.csv",
  "extreme_returns.csv",
  "correlation_levels.csv",
  "correlation_returns.csv",
  "rolling_correlation_60d.csv",
  "rolling_correlation_250d.csv",
  "ljung_box_returns.csv",
  "arch_lm_returns.csv",
  "spread_summary.csv",
  "spread_levels_long.csv",
  "stats_returns_by_window.csv",
  "stats_spreads_by_window.csv",
  "correlation_returns_by_window.csv",
  "volatility_by_year.csv",
  "stats_returns_by_month.csv"
)

missing_model_files <- required_model_files[
  !file.exists(file.path(model_dir, required_model_files))
]

if (length(missing_model_files) > 0) {
  stop(
    "Missing descriptive analysis output files:\n",
    paste(missing_model_files, collapse = "\n")
  )
}

balanced_file <- file.path(clean_dir, "gas_clean_balanced.csv")
ma3_file      <- file.path(clean_dir, "gas_clean_ma3_balanced.csv")

if (!file.exists(balanced_file)) {
  stop("Missing balanced data file: ", balanced_file)
}

if (!file.exists(ma3_file)) {
  stop("Missing MA3 balanced data file: ", ma3_file)
}

# Load descriptive statistics

gas <- read_csv(balanced_file, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

gas_ma3 <- read_csv(ma3_file, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

stats_levels      <- read_csv(file.path(model_dir, "stats_levels.csv"), show_col_types = FALSE)
stats_log_levels  <- read_csv(file.path(model_dir, "stats_log_levels.csv"), show_col_types = FALSE)
stats_returns     <- read_csv(file.path(model_dir, "stats_log_returns.csv"), show_col_types = FALSE)
log_levels_long   <- read_csv(file.path(model_dir, "log_levels_long.csv"), show_col_types = FALSE) |> mutate(Date = as.Date(Date))
returns_long      <- read_csv(file.path(model_dir, "log_returns_long.csv"), show_col_types = FALSE) |> mutate(Date = as.Date(Date))
extreme_returns   <- read_csv(file.path(model_dir, "extreme_returns.csv"), show_col_types = FALSE) |> mutate(Date = as.Date(Date))
cor_levels        <- read_csv(file.path(model_dir, "correlation_levels.csv"), show_col_types = FALSE)
cor_returns       <- read_csv(file.path(model_dir, "correlation_returns.csv"), show_col_types = FALSE)
rolling_60        <- read_csv(file.path(model_dir, "rolling_correlation_60d.csv"), show_col_types = FALSE) |> mutate(Date = as.Date(Date))
rolling_250       <- read_csv(file.path(model_dir, "rolling_correlation_250d.csv"), show_col_types = FALSE) |> mutate(Date = as.Date(Date))
lb_results        <- read_csv(file.path(model_dir, "ljung_box_returns.csv"), show_col_types = FALSE)
arch_results      <- read_csv(file.path(model_dir, "arch_lm_returns.csv"), show_col_types = FALSE)
spread_summary    <- read_csv(file.path(model_dir, "spread_summary.csv"), show_col_types = FALSE)
spread_long       <- read_csv(file.path(model_dir, "spread_levels_long.csv"), show_col_types = FALSE) |> mutate(Date = as.Date(Date))
returns_by_window <- read_csv(file.path(model_dir, "stats_returns_by_window.csv"), show_col_types = FALSE)
spreads_by_window <- read_csv(file.path(model_dir, "stats_spreads_by_window.csv"), show_col_types = FALSE)
cor_by_window     <- read_csv(file.path(model_dir, "correlation_returns_by_window.csv"), show_col_types = FALSE)
vol_by_year       <- read_csv(file.path(model_dir, "volatility_by_year.csv"), show_col_types = FALSE)
stats_by_month    <- read_csv(file.path(model_dir, "stats_returns_by_month.csv"), show_col_types = FALSE)

# Definitions and validation

gas_hubs <- c("JKM", "NBP", "TTF", "HH")
all_hubs <- c(gas_hubs, "Brent_crude")
ma3_cols <- c("Date", "Brent_crude", "Brent_MA3")

if (length(setdiff(c("Date", all_hubs), names(gas))) > 0) {
  stop("gas_clean_balanced.csv is missing one or more required columns.")
}

if (length(setdiff(ma3_cols, names(gas_ma3))) > 0) {
  stop("gas_clean_ma3_balanced.csv is missing Date, Brent_crude, or Brent_MA3.")
}

if (anyNA(gas[all_hubs])) {
  stop("gas_clean_balanced.csv contains missing values.")
}

if (any(gas[all_hubs] <= 0, na.rm = TRUE)) {
  stop("gas_clean_balanced.csv contains non positive prices.")
}

if (anyNA(gas_ma3[c("Brent_crude", "Brent_MA3")])) {
  stop("gas_clean_ma3_balanced.csv contains missing Brent values.")
}

if (any(gas_ma3[c("Brent_crude", "Brent_MA3")] <= 0, na.rm = TRUE)) {
  stop("gas_clean_ma3_balanced.csv contains non positive Brent values.")
}

hub_colors <- c(
  "JKM" = "#1B1B3A",
  "TTF" = "#1F78B4",
  "NBP" = "#33A02C",
  "HH" = "#FF7F00",
  "Brent_crude" = "#B22182",
  "Brent_MA3" = "#8A4F7D"
)

hub_labels <- c(
  "JKM" = "JKM",
  "TTF" = "TTF",
  "NBP" = "NBP",
  "HH" = "Henry Hub",
  "Brent_crude" = "Brent spot",
  "Brent_MA3" = "Brent spot MA(63)"
)

window_order <- c(
  "Pre-COVID",
  "COVID shock",
  "Interim recovery",
  "Energy crisis (pre-war)",
  "Post-invasion crisis",
  "Normalization"
)

sample_note <- paste0(
  "Sample: daily balanced observations, ",
  format(min(gas$Date), "%d %b %Y"),
  " to ",
  format(max(gas$Date), "%d %b %Y"),
  "."
)

stars_note <- "***, **, * denote rejection of the null at the 1%, 5%, and 10% levels, respectively."

# LaTeX-friendly plotting helpers

fig_full_width  <- 7.20
fig_flat_height <- 3.35
fig_std_height  <- 3.85
fig_tall_height <- 5.80
fig_app_height  <- 6.40

theme_thesis <- function(base_size = 9.5) {
  theme_bw(base_family = "serif", base_size = base_size) +
    theme(
      text = element_text(color = "black"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.25),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.30),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.20),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.30),
      strip.text = element_text(size = base_size, margin = margin(2, 2, 2, 2)),
      legend.position = "none",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.height = unit(0.35, "cm"),
      legend.key.width = unit(0.75, "cm"),
      plot.margin = margin(3, 4, 3, 3)
    )
}

save_plot <- function(plot, filename, width = fig_full_width, height = fig_std_height, dpi = 300) {
  ggsave(
    filename = file.path(plots_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}

format_table <- function(x) {
  x |>
    tab_options(
      table.font.names = "Times New Roman",
      table.font.size = 13,
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
      source_notes.font.size = 11
    ) |>
    opt_table_font(font = list("Times New Roman", "Times", "serif"))
}

save_gt_png <- function(gt_object, filename) {
  gt::gtsave(gt_object, file.path(tables_dir, filename))
}

add_stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

# 1. Log price levels

p_log_levels <- log_levels_long |>
  filter(Hub %in% all_hubs) |>
  mutate(Hub = factor(Hub, levels = all_hubs)) |>
  ggplot(aes(Date, Log_Price, color = Hub)) +
  geom_line(linewidth = 0.38) +
  scale_color_manual(values = hub_colors, labels = hub_labels) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = NULL, y = "log price") +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot(p_log_levels, "Log_price_levels.png", height = fig_std_height)

# 2. Price levels, combined thermal equivalent

event_windows <- tibble::tribble(
  ~Event, ~Start, ~End,
  "COVID-19 shock",          as.Date("2020-02-01"), as.Date("2020-06-30"),
  "European energy crisis",  as.Date("2021-09-01"), as.Date("2022-12-31")
)

event_labels <- event_windows |>
  mutate(
    Label_date = Start + (End - Start) / 2,
    y = 97
  )

reference_lines <- tibble::tribble(
  ~Event, ~Date,
  "U.S. LNG exports begin",                    as.Date("2016-02-24"),
  "TTF overtakes NBP as leading European hub", as.Date("2016-07-01"),
  "Russia's full-scale invasion of Ukraine",   as.Date("2022-02-24"),
  "Middle East / Hormuz risk",                 as.Date("2026-02-28")
)

line_callouts <- tibble::tribble(
  ~Label,                                      ~Date,                  ~Label_date,             ~y, ~yend,
  "U.S. LNG exports begin",                   as.Date("2016-02-24"), as.Date("2016-10-01"),  14,   5,
  "TTF overtakes NBP\nas leading hub",         as.Date("2016-07-01"), as.Date("2017-04-01"),  27,   8,
  "Russia invades\nUkraine",                  as.Date("2022-02-24"), as.Date("2021-07-01"),  55,  28,
  "Middle East /\nHormuz risk",                as.Date("2026-02-28"), as.Date("2025-04-01"),  30,  10
)

levels_long <- gas |>
  select(Date, all_of(all_hubs)) |>
  pivot_longer(-Date, names_to = "Hub", values_to = "Price") |>
  mutate(Hub = factor(Hub, levels = all_hubs))

p_price_levels <- levels_long |>
  filter(Hub %in% all_hubs) |>
  ggplot(aes(Date, Price, color = Hub)) +
  geom_rect(
    data = event_windows,
    aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.20
  ) +
  geom_vline(
    data = reference_lines,
    aes(xintercept = Date),
    inherit.aes = FALSE,
    linetype = "dashed",
    colour = "grey35",
    linewidth = 0.28
  ) +
  geom_line(linewidth = 0.38) +
  geom_text(
    data = event_labels,
    aes(x = Label_date, y = y, label = Event),
    inherit.aes = FALSE,
    size = 2.35,
    family = "serif",
    colour = "grey15",
    fontface = "bold"
  ) +
  geom_segment(
    data = line_callouts,
    aes(x = Label_date, xend = Date, y = y, yend = yend),
    inherit.aes = FALSE,
    colour = "grey35",
    linewidth = 0.20
  ) +
  geom_label(
    data = line_callouts,
    aes(x = Label_date, y = y, label = Label),
    inherit.aes = FALSE,
    size = 2.20,
    family = "serif",
    colour = "grey20",
    fill = "white",
    label.size = 0.14,
    label.padding = unit(0.08, "lines")
  ) +
  scale_color_manual(values = hub_colors, labels = hub_labels) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.025))
  ) +
  scale_y_continuous(
    limits = c(0, 106),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0.00, 0.01))
  ) +
  labs(x = NULL, y = "USD/MMBtu") +
  coord_cartesian(clip = "off") +
  theme_thesis(base_size = 9.0) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.margin = margin(3, 8, 3, 3)
  )

save_plot(
  p_price_levels,
  "Price_levels_combined.png",
  width = fig_full_width,
  height = fig_flat_height
)

# 3. Brent spot and Brent spot MA(63), thermal equivalent

brent_ma3_long <- gas_ma3 |>
  select(Date, Brent_crude, Brent_MA3) |>
  pivot_longer(-Date, names_to = "Series", values_to = "Price") |>
  mutate(Series = factor(Series, levels = c("Brent_crude", "Brent_MA3")))

p_brent_ma3 <- brent_ma3_long |>
  ggplot(aes(Date, Price, color = Series, linetype = Series)) +
  geom_line(linewidth = 0.42) +
  scale_color_manual(values = hub_colors, labels = hub_labels) +
  scale_linetype_manual(
    values = c("Brent_crude" = "solid", "Brent_MA3" = "dashed"),
    labels = hub_labels
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = NULL, y = "USD/MMBtu") +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot(p_brent_ma3, "Brent_spot_and_MA63.png", height = fig_flat_height)

# 4. Log returns

p_returns <- returns_long |>
  filter(Hub %in% all_hubs) |>
  mutate(Hub = factor(Hub, levels = all_hubs)) |>
  ggplot(aes(Date, Value, color = Hub)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.25) +
  geom_line(linewidth = 0.20) +
  facet_wrap(~Hub, ncol = 1, labeller = as_labeller(hub_labels)) +
  scale_color_manual(values = hub_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = NULL, y = "Log return") +
  theme_thesis(base_size = 8.8)

save_plot(p_returns, "Log_returns.png", height = 6.20)

# 5. Return distributions

normal_overlay <- stats_returns |>
  filter(Hub %in% gas_hubs) |>
  select(Hub, Mean, SD) |>
  tidyr::expand_grid(x = seq(-0.30, 0.30, length.out = 400)) |>
  mutate(
    density = dnorm(x, Mean, SD),
    Hub = factor(Hub, levels = gas_hubs)
  )

p_dist <- ggplot() +
  geom_histogram(
    data = returns_long |>
      filter(Hub %in% gas_hubs) |>
      mutate(Hub = factor(Hub, levels = gas_hubs)),
    aes(Value, after_stat(density), fill = Hub),
    bins = 90,
    alpha = 0.35,
    color = NA
  ) +
  geom_density(
    data = returns_long |>
      filter(Hub %in% gas_hubs) |>
      mutate(Hub = factor(Hub, levels = gas_hubs)),
    aes(Value, color = Hub),
    linewidth = 0.55
  ) +
  geom_line(
    data = normal_overlay,
    aes(x, density),
    linetype = "dashed",
    color = "black",
    linewidth = 0.35
  ) +
  facet_wrap(~Hub, scales = "free_y", ncol = 2, labeller = as_labeller(hub_labels)) +
  scale_fill_manual(values = hub_colors) +
  scale_color_manual(values = hub_colors) +
  coord_cartesian(xlim = c(-0.25, 0.25)) +
  labs(x = "Log return", y = "Density") +
  theme_thesis(base_size = 9.0)

save_plot(p_dist, "Return_distributions.png", height = fig_tall_height)

# 6. Annual volatility

p_vol_year <- vol_by_year |>
  filter(Hub %in% all_hubs) |>
  mutate(Hub = factor(Hub, levels = all_hubs)) |>
  ggplot(aes(Year, Annualized_SD, color = Hub, group = Hub)) +
  geom_line(linewidth = 0.42) +
  geom_point(size = 1.1) +
  scale_color_manual(values = hub_colors, labels = hub_labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Annualised volatility") +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot(p_vol_year, "Volatility_by_year.png", height = fig_flat_height)

# 7. Return volatility by crisis window

p_vol_window <- returns_by_window |>
  filter(Hub %in% gas_hubs) |>
  mutate(
    Hub = factor(Hub, levels = gas_hubs),
    Window = factor(Window, levels = window_order)
  ) |>
  ggplot(aes(Window, Annualized_SD, fill = Hub)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_fill_manual(values = hub_colors, labels = hub_labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Annualised volatility") +
  theme_thesis(base_size = 9.0) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot(p_vol_window, "Return_volatility_by_window.png", height = 4.30)

# 8. Gas gas spreads

p_spreads <- spread_long |>
  ggplot(aes(Date, Spread)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.25) +
  geom_line(linewidth = 0.28, color = "#1B1B3A") +
  facet_wrap(~Pair, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = NULL, y = "Log spread") +
  theme_thesis(base_size = 8.8)

save_plot(p_spreads, "Gas_spreads.png", height = fig_app_height)

# 9. Spread severity by crisis window

p_spread_window <- spreads_by_window |>
  mutate(Window = factor(Window, levels = window_order)) |>
  ggplot(aes(Window, Mean_Abs_Spread)) +
  geom_col(fill = "#4D4D4D", width = 0.62) +
  facet_wrap(~Pair, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Mean absolute log spread") +
  theme_thesis(base_size = 8.8) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_plot(p_spread_window, "Spread_severity_by_window.png", height = fig_app_height)

# 10. Return correlation heatmap

make_cor_heatmap <- function(cor_df, filename) {
  order <- cor_df$Hub

  cor_long <- cor_df |>
    pivot_longer(-Hub, names_to = "Hub2", values_to = "Correlation") |>
    mutate(
      Hub = factor(Hub, levels = order),
      Hub2 = factor(Hub2, levels = rev(order))
    )

  p <- cor_long |>
    ggplot(aes(Hub, Hub2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.45) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 2.55, family = "serif") +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    scale_x_discrete(labels = hub_labels) +
    scale_y_discrete(labels = hub_labels) +
    coord_fixed() +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_thesis(base_size = 9.0) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(3, 3, 3, 3)
    )

  save_plot(p, filename, width = 5.70, height = 4.90)
}

make_cor_heatmap(cor_returns, "Return_correlation_heatmap.png")

# 11. Rolling gas gas correlations

plot_rolling_cor <- function(df, filename) {
  p <- df |>
    ggplot(aes(Date, Correlation)) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.25) +
    geom_line(linewidth = 0.35, color = "#1B1B3A") +
    facet_wrap(~Pair, ncol = 2) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(x = NULL, y = "Rolling return correlation") +
    theme_thesis(base_size = 8.8)

  save_plot(p, filename, height = fig_app_height)
}

plot_rolling_cor(rolling_250, "Rolling_correlations_250d.png")
plot_rolling_cor(rolling_60,  "Rolling_correlations_60d.png")

# 12. Return correlations by crisis window

p_cor_window <- cor_by_window |>
  mutate(
    Pair = paste(Hub, Hub2, sep = "-"),
    Window = factor(Window, levels = window_order)
  ) |>
  ggplot(aes(Window, Correlation)) +
  geom_col(fill = "#4D4D4D", width = 0.62) +
  facet_wrap(~Pair, ncol = 2) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = NULL, y = "Return correlation") +
  theme_thesis(base_size = 8.8) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_plot(p_cor_window, "Return_correlations_by_window.png", height = fig_app_height)

# 13. Monthly return seasonality

p_month <- stats_by_month |>
  filter(Hub %in% gas_hubs) |>
  mutate(
    Hub = factor(Hub, levels = gas_hubs),
    Month_Name = factor(Month_Name, levels = month.abb)
  ) |>
  ggplot(aes(Month_Name, SD, fill = Hub)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  scale_fill_manual(values = hub_colors, labels = hub_labels) +
  labs(x = NULL, y = "Daily return standard deviation") +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

save_plot(p_month, "Monthly_return_volatility.png", height = fig_flat_height)

# 14. Summary statistics tables

make_stats_table <- function(df, filename, decimals_main = 3) {
  out <- df |>
    mutate(
      Hub = hub_labels[Hub],
      JB_sig = add_stars(JB_p)
    )

  keep_cols <- c(
    "Hub", "N", "Mean", "SD", "Annualized_SD", "Min", "P5",
    "Median", "P95", "Max", "Skew", "Kurt", "JB_stat", "JB_sig"
  )

  out <- out[, intersect(keep_cols, names(out))]

  labels <- list(
    Hub = "Series",
    N = "N",
    Mean = "Mean",
    SD = "S.D.",
    Annualized_SD = "Ann. S.D.",
    Min = "Min.",
    P5 = "P5",
    Median = "Median",
    P95 = "P95",
    Max = "Max.",
    Skew = "Skew.",
    Kurt = "Ex. kurt.",
    JB_stat = "JB",
    JB_sig = ""
  )

  g <- out |>
    gt() |>
    cols_label(.list = labels[names(labels) %in% names(out)]) |>
    fmt_number(columns = any_of("N"), decimals = 0, use_seps = TRUE) |>
    fmt_number(
      columns = any_of(c("Mean", "SD", "Annualized_SD", "Min", "P5", "Median", "P95", "Max")),
      decimals = decimals_main
    ) |>
    fmt_number(columns = any_of(c("Skew", "Kurt")), decimals = 2) |>
    fmt_number(columns = any_of("JB_stat"), decimals = 1) |>
    cols_align(align = "left", columns = any_of(c("Hub", "JB_sig"))) |>
    cols_align(align = "right", columns = where(is.numeric)) |>
    tab_source_note(sample_note) |>
    tab_source_note(stars_note) |>
    format_table()

  save_gt_png(g, filename)
}

make_stats_table(stats_levels, "Stats_levels.png", decimals_main = 2)
make_stats_table(stats_log_levels, "Stats_log_levels.png", decimals_main = 3)
make_stats_table(stats_returns, "Stats_returns.png", decimals_main = 4)

# 15. Spread summary table

spread_table <- spread_summary |>
  select(
    Pair, N, Mean, SD, Mean_Abs_Spread,
    P5, Median, P95, Pct_above_0.5, Pct_above_1.0
  ) |>
  gt() |>
  cols_label(
    Pair = "Pair",
    N = "N",
    Mean = "Mean",
    SD = "S.D.",
    Mean_Abs_Spread = "Mean abs.",
    P5 = "P5",
    Median = "Median",
    P95 = "P95",
    `Pct_above_0.5` = "|spread| > 0.5",
    `Pct_above_1.0` = "|spread| > 1.0"
  ) |>
  fmt_number(columns = N, decimals = 0, use_seps = TRUE) |>
  fmt_number(columns = c(Mean, SD, Mean_Abs_Spread, P5, Median, P95), decimals = 3) |>
  fmt_number(columns = c(`Pct_above_0.5`, `Pct_above_1.0`), decimals = 1) |>
  cols_align(align = "left", columns = Pair) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_source_note("Spreads are log price differences between gas hubs.") |>
  tab_source_note("Thresholds 0.5 and 1.0 correspond to approximate price ratio gaps of 65% and 172%.") |>
  format_table()

save_gt_png(spread_table, "Spread_summary.png")

# 16. Diagnostics table

diag_table <- lb_results |>
  left_join(arch_results, by = "Hub") |>
  mutate(
    Hub = hub_labels[Hub],
    LB_p10_sig = add_stars(LB_p10),
    LB_p20_sig = add_stars(LB_p20),
    ARCH_p_sig = add_stars(ARCH_LM_p)
  ) |>
  gt() |>
  cols_label(
    Hub = "Series",
    LB_Q10 = "Q(10)",
    LB_p10 = "p",
    LB_p10_sig = "",
    LB_Q20 = "Q(20)",
    LB_p20 = "p",
    LB_p20_sig = "",
    ARCH_LM_stat = "LM(10)",
    ARCH_LM_p = "p",
    ARCH_p_sig = ""
  ) |>
  fmt_number(columns = c(LB_Q10, LB_Q20, ARCH_LM_stat), decimals = 2) |>
  fmt_number(columns = c(LB_p10, LB_p20, ARCH_LM_p), decimals = 3) |>
  cols_align(align = "left", columns = c(Hub, LB_p10_sig, LB_p20_sig, ARCH_p_sig)) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_spanner(
    label = "Ljung-Box",
    columns = c(LB_Q10, LB_p10, LB_p10_sig, LB_Q20, LB_p20, LB_p20_sig)
  ) |>
  tab_spanner(
    label = "ARCH-LM",
    columns = c(ARCH_LM_stat, ARCH_LM_p, ARCH_p_sig)
  ) |>
  tab_source_note("Ljung-Box tests the null of no return autocorrelation. ARCH-LM tests the null of no conditional heteroskedasticity.") |>
  tab_source_note(sample_note) |>
  tab_source_note(stars_note) |>
  format_table()

save_gt_png(diag_table, "Return_diagnostics.png")

# 17. Extreme returns table

extreme_table <- extreme_returns |>
  filter(Hub %in% gas_hubs) |>
  mutate(Hub = hub_labels[Hub]) |>
  arrange(Hub, Direction, Rank) |>
  gt(groupname_col = "Hub") |>
  cols_label(
    Direction = "Direction",
    Rank = "Rank",
    Date = "Date",
    Log_Return = "Log return"
  ) |>
  fmt_date(columns = Date, date_style = "iso") |>
  fmt_number(columns = Log_Return, decimals = 3) |>
  cols_align(align = "left", columns = Direction) |>
  cols_align(align = "right", columns = c(Rank, Date, Log_Return)) |>
  tab_source_note("Reports the ten largest positive and ten largest negative daily log returns per gas hub.") |>
  format_table()

save_gt_png(extreme_table, "Extreme_returns.png")

# Final check

expected_plots <- file.path(
  plots_dir,
  c(
    "Log_price_levels.png",
    "Price_levels_combined.png",
    "Brent_spot_and_MA63.png",
    "Log_returns.png",
    "Return_distributions.png",
    "Volatility_by_year.png",
    "Return_volatility_by_window.png",
    "Gas_spreads.png",
    "Spread_severity_by_window.png",
    "Return_correlation_heatmap.png",
    "Rolling_correlations_250d.png",
    "Rolling_correlations_60d.png",
    "Return_correlations_by_window.png",
    "Monthly_return_volatility.png"
  )
)

expected_tables <- file.path(
  tables_dir,
  c(
    "Stats_levels.png",
    "Stats_log_levels.png",
    "Stats_returns.png",
    "Spread_summary.png",
    "Return_diagnostics.png",
    "Extreme_returns.png"
  )
)

expected_outputs <- c(expected_plots, expected_tables)
missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

cat("\n== Descriptive Analysis Visualisation Finished ==\n")
cat("   Plot directory:", plots_dir, "\n")
cat("   Table directory:", tables_dir, "\n")
cat("   PNG outputs created:", sum(file.exists(expected_outputs)), "of", length(expected_outputs), "\n")

if (length(missing_outputs) > 0) {
  cat("   PNG outputs not created:\n")
  cat(paste0("     ", basename(missing_outputs), collapse = "\n"), "\n")
}