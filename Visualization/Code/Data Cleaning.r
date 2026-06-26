# Data Cleaning Visualisation
# Input: gas_clean_raw_aligned.csv, gas_clean_balanced.csv, gas_clean_ma3_balanced.csv
# Output: missing data maps, sample construction, Brent_MA3 audit plots

pkgs <- c(
  "dplyr", "tidyr", "ggplot2", "readr", "gt", "here", "tibble",
  "scales", "webshot2"
)

to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

project_dir <- here::here()

clean_dir <- file.path(project_dir, "Model", "model output", "Data Cleaning")
out_dir   <- file.path(project_dir, "Visualization", "Plot outputs", "Data Cleaning")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

files <- list(
  raw      = file.path(clean_dir, "gas_clean_raw_aligned.csv"),
  balanced = file.path(clean_dir, "gas_clean_balanced.csv"),
  ma3      = file.path(clean_dir, "gas_clean_ma3_balanced.csv")
)

missing_files <- unlist(files)[!file.exists(unlist(files))]
if (length(missing_files) > 0) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

raw <- read_csv(files$raw, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

balanced <- read_csv(files$balanced, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

balanced_ma3 <- read_csv(files$ma3, show_col_types = FALSE) |>
  mutate(Date = as.Date(Date)) |>
  arrange(Date)

gas_series  <- c("JKM", "NBP", "TTF", "HH")
core_series <- c(gas_series, "Brent_crude")
ma3_series  <- c(core_series, "Brent_MA3")

has_brent_spot <- "Brent_spot" %in% names(raw)

plot_series <- c(gas_series, "Brent_crude", "Brent_MA3")
if (has_brent_spot) {
  plot_series <- c(gas_series, "Brent_crude", "Brent_spot", "Brent_MA3")
}
plot_series <- plot_series[plot_series %in% names(raw)]

required_raw <- c("Date", core_series, "Brent_MA3")
if (has_brent_spot) {
  required_raw <- c(required_raw, "Brent_spot")
}

required_balanced <- c("Date", core_series)
required_ma3      <- c("Date", ma3_series)

missing_raw_cols <- setdiff(required_raw, names(raw))
missing_bal_cols <- setdiff(required_balanced, names(balanced))
missing_ma3_cols <- setdiff(required_ma3, names(balanced_ma3))

if (length(missing_raw_cols) > 0) {
  stop(
    "Raw aligned file is missing required columns:\n",
    paste(missing_raw_cols, collapse = ", "),
    "\nRe-run the revised data cleaning script."
  )
}

if (length(missing_bal_cols) > 0) {
  stop(
    "Balanced file is missing required columns:\n",
    paste(missing_bal_cols, collapse = ", ")
  )
}

if (length(missing_ma3_cols) > 0) {
  stop(
    "MA3 balanced file is missing required columns:\n",
    paste(missing_ma3_cols, collapse = ", "),
    "\nRe-run the revised data cleaning script."
  )
}

if (anyNA(balanced[core_series])) {
  stop("Balanced file contains missing values in the core series.")
}

if (anyNA(balanced_ma3[ma3_series])) {
  stop("MA3 balanced file contains missing values in the MA3 series.")
}

if (any(balanced[core_series] <= 0, na.rm = TRUE)) {
  stop("Balanced file contains non-positive prices.")
}

if (any(balanced_ma3[ma3_series] <= 0, na.rm = TRUE)) {
  stop("MA3 balanced file contains non-positive prices.")
}

if (nrow(balanced_ma3) > nrow(balanced)) {
  stop("MA3 balanced panel has more rows than the core balanced panel. Check cleaning logic.")
}

# Visual identity

series_order <- c("JKM", "TTF", "NBP", "HH", "Brent_crude", "Brent_spot", "Brent_MA3")
series_order <- series_order[series_order %in% plot_series]

series_labels <- c(
  "JKM"         = "JKM",
  "TTF"         = "TTF",
  "NBP"         = "NBP",
  "HH"          = "Henry Hub",
  "Brent_crude" = "Brent crude",
  "Brent_spot"  = "Brent spot input",
  "Brent_MA3"   = "Brent spot MA(63)"
)

series_colors <- c(
  "JKM"         = "#1B1B3A",
  "TTF"         = "#1F78B4",
  "NBP"         = "#33A02C",
  "HH"          = "#FF7F00",
  "Brent_crude" = "#B22182",
  "Brent_spot"  = "#8A4F7D",
  "Brent_MA3"   = "#4D4D4D"
)

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

# Shared data

raw_long <- raw |>
  pivot_longer(
    cols = all_of(series_order),
    names_to = "Series",
    values_to = "Price"
  ) |>
  mutate(
    Series = factor(Series, levels = series_order),
    Status = if_else(is.na(Price), "Missing", "Available"),
    Year = as.integer(format(Date, "%Y"))
  )

# 1. Missing data map

p_missing <- raw_long |>
  mutate(Series = factor(as.character(Series), levels = rev(series_order))) |>
  ggplot(aes(Date, Series, fill = Status)) +
  geom_tile() +
  scale_fill_manual(values = c("Available" = "#E0E0E0", "Missing" = "#B2182B")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(0, 0)) +
  scale_y_discrete(labels = series_labels) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_thesis() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm")
  )

ggsave(
  file.path(out_dir, "Missing_data_map.png"),
  p_missing,
  width = 10,
  height = 4.2,
  dpi = 300
)

# 2. Data availability table

availability <- raw_long |>
  group_by(Series) |>
  summarise(
    Start = ifelse(
      any(!is.na(Price)),
      as.character(min(Date[!is.na(Price)], na.rm = TRUE)),
      NA_character_
    ),
    End = ifelse(
      any(!is.na(Price)),
      as.character(max(Date[!is.na(Price)], na.rm = TRUE)),
      NA_character_
    ),
    Observed = sum(!is.na(Price)),
    Missing = sum(is.na(Price)),
    Pct_missing = 100 * Missing / n(),
    .groups = "drop"
  ) |>
  mutate(
    Start = as.Date(Start),
    End = as.Date(End),
    Series = as.character(Series),
    Series = series_labels[Series]
  )

gt_availability <- availability |>
  gt() |>
  cols_label(
    Series = "Series",
    Start = "Start",
    End = "End",
    Observed = "N observed",
    Missing = "Missing",
    Pct_missing = "% missing"
  ) |>
  fmt_date(columns = c(Start, End), date_style = "iso") |>
  fmt_number(columns = c(Observed, Missing), decimals = 0, use_seps = TRUE) |>
  fmt_number(columns = Pct_missing, decimals = 1) |>
  cols_align(align = "left", columns = Series) |>
  cols_align(align = "right", columns = where(is.numeric)) |>
  tab_source_note(
    "Availability is computed from gas_clean_raw_aligned.csv before complete-case filtering."
  ) |>
  tab_source_note(
    "Brent_MA3 is a 63-business-day trailing moving average constructed from the separate Brent spot input file."
  ) |>
  format_table()

save_gt_png(gt_availability, file.path(out_dir, "Data_availability.png"))

# 3. Sample contract table

sample_contract <- tibble(
  File = c(
    "gas_clean_raw_aligned.csv",
    "gas_clean_balanced.csv",
    "gas_clean_ma3_balanced.csv"
  ),
  Role = c(
    "Audit panel",
    "Core baseline estimation panel",
    "Oil MA(63) estimation panel"
  ),
  Sample_rule = c(
    "Common gas and core Brent dates after joining. Hub prices and MA(63) may be missing.",
    "Date, JKM, NBP, TTF, HH, and Brent_crude observed.",
    "Date, JKM, NBP, TTF, HH, Brent_crude, and Brent_MA3 observed."
  ),
  Observations = c(nrow(raw), nrow(balanced), nrow(balanced_ma3)),
  Start = c(min(raw$Date), min(balanced$Date), min(balanced_ma3$Date)),
  End = c(max(raw$Date), max(balanced$Date), max(balanced_ma3$Date))
)

gt_contract <- sample_contract |>
  gt() |>
  cols_label(
    File = "File",
    Role = "Role",
    Sample_rule = "Sample rule",
    Observations = "N",
    Start = "Start",
    End = "End"
  ) |>
  fmt_number(columns = Observations, decimals = 0, use_seps = TRUE) |>
  fmt_date(columns = c(Start, End), date_style = "iso") |>
  cols_align(align = "left", columns = c(File, Role, Sample_rule)) |>
  cols_align(align = "right", columns = c(Observations, Start, End)) |>
  tab_source_note(
    "No-oil models should use gas_clean_balanced.csv. Oil models using Brent_MA3 should use gas_clean_ma3_balanced.csv."
  ) |>
  format_table()

save_gt_png(gt_contract, file.path(out_dir, "Sample_contract.png"))

# 4. Missingness by series

missing_by_series <- raw_long |>
  group_by(Series) |>
  summarise(
    Missing = sum(is.na(Price)),
    Pct_missing = 100 * Missing / n(),
    .groups = "drop"
  )

p_missing_series <- missing_by_series |>
  ggplot(aes(Series, Pct_missing)) +
  geom_col(fill = "#4D4D4D", width = 0.65) +
  scale_x_discrete(labels = series_labels) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(x = NULL, y = "Missing observations") +
  theme_thesis() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

ggsave(
  file.path(out_dir, "Missingness_by_series.png"),
  p_missing_series,
  width = 7.8,
  height = 3.8,
  dpi = 300
)

# 5. Sample attrition table

core_complete_raw <- complete.cases(raw[core_series])

balanced_ma3_dates <- balanced_ma3 |>
  distinct(Date) |>
  mutate(In_MA3_panel = TRUE)

balanced_with_ma3_flag <- balanced |>
  distinct(Date) |>
  left_join(balanced_ma3_dates, by = "Date") |>
  mutate(In_MA3_panel = if_else(is.na(In_MA3_panel), FALSE, In_MA3_panel))

rows_removed_core <- nrow(raw) - sum(core_complete_raw)
rows_removed_ma3  <- nrow(balanced) - nrow(balanced_ma3)

sample_attrition <- tibble(
  Step = c(
    "Raw aligned panel",
    "Rows removed for core balanced panel",
    "Core balanced panel",
    "Rows removed for MA(63) balanced panel",
    "MA(63) balanced panel"
  ),
  Definition = c(
    "Common gas and core Brent dates after joining.",
    "Rows with at least one missing value in JKM, NBP, TTF, HH, or Brent_crude.",
    "Complete observations for JKM, NBP, TTF, HH, and Brent_crude.",
    "Additional rows removed because Brent_MA3 is missing.",
    "Complete observations for core series and Brent_MA3."
  ),
  Observations = c(
    nrow(raw),
    rows_removed_core,
    nrow(balanced),
    rows_removed_ma3,
    nrow(balanced_ma3)
  )
)

gt_attrition <- sample_attrition |>
  gt() |>
  cols_label(
    Step = "Step",
    Definition = "Definition",
    Observations = "N"
  ) |>
  fmt_number(columns = Observations, decimals = 0, use_seps = TRUE) |>
  cols_align(align = "left", columns = c(Step, Definition)) |>
  cols_align(align = "right", columns = Observations) |>
  tab_source_note(
    "The MA(63) sample is shorter only if the trailing Brent moving average is unavailable at the beginning or end of the model sample."
  ) |>
  format_table()

save_gt_png(gt_attrition, file.path(out_dir, "Sample_attrition.png"))

# 6. Missingness by year heatmap

missing_by_year <- raw_long |>
  group_by(Series, Year) |>
  summarise(
    Pct_missing = 100 * sum(is.na(Price)) / n(),
    .groups = "drop"
  ) |>
  mutate(Series = factor(as.character(Series), levels = rev(series_order)))

p_missing_year <- missing_by_year |>
  ggplot(aes(factor(Year), Series, fill = Pct_missing)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_y_discrete(labels = series_labels) +
  scale_fill_gradient(
    low = "#F2F2F2",
    high = "#B2182B",
    labels = function(x) paste0(round(x, 1), "%")
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_thesis() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(0.28, "cm")
  )

ggsave(
  file.path(out_dir, "Missingness_by_year.png"),
  p_missing_year,
  width = 8.5,
  height = 4.0,
  dpi = 300
)

# 7. Brent_MA3 audit plot

if (has_brent_spot) {
  brent_audit <- raw |>
    select(Date, Brent_spot, Brent_MA3) |>
    pivot_longer(
      cols = c(Brent_spot, Brent_MA3),
      names_to = "Series",
      values_to = "Price"
    ) |>
    mutate(
      Series = factor(
        Series,
        levels = c("Brent_spot", "Brent_MA3"),
        labels = c("Brent spot", "Brent MA(63)")
      )
    )

  p_brent <- brent_audit |>
    ggplot(aes(Date, Price, color = Series, linetype = Series)) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    scale_color_manual(
      values = c(
        "Brent spot" = series_colors["Brent_spot"],
        "Brent MA(63)" = series_colors["Brent_MA3"]
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Brent spot" = "dotted",
        "Brent MA(63)" = "solid"
      )
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(0.01, 0.01)) +
    labs(x = NULL, y = "USD/mmBtu", color = NULL, linetype = NULL) +
    theme_thesis() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    file.path(out_dir, "Brent_MA3_audit.png"),
    p_brent,
    width = 9,
    height = 4.8,
    dpi = 300
  )
} else {
  brent_audit <- raw |>
    select(Date, Brent_MA3)

  p_brent <- brent_audit |>
    ggplot(aes(Date, Brent_MA3)) +
    geom_line(color = series_colors["Brent_MA3"], linewidth = 0.6, na.rm = TRUE) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(0.01, 0.01)) +
    labs(x = NULL, y = "Brent MA(63), USD/mmBtu") +
    theme_thesis()

  ggsave(
    file.path(out_dir, "Brent_MA3_audit.png"),
    p_brent,
    width = 9,
    height = 4.8,
    dpi = 300
  )
}

# Final check

expected_outputs <- file.path(
  out_dir,
  c(
    "Missing_data_map.png",
    "Data_availability.png",
    "Sample_contract.png",
    "Missingness_by_series.png",
    "Sample_attrition.png",
    "Missingness_by_year.png",
    "Brent_MA3_audit.png"
  )
)

missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

cat("\n== Data Cleaning Visualisation Finished ==\n")
cat("   Output directory:", out_dir, "\n")
cat("   PNG outputs created:", sum(file.exists(expected_outputs)), "of", length(expected_outputs), "\n")

if (length(missing_outputs) > 0) {
  cat("   PNG outputs not created:\n")
  cat(paste0("     ", basename(missing_outputs), collapse = "\n"), "\n")
}