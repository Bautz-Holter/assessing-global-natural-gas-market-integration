# Data Cleaning
# Input: Gas prices.xlsx, brent_crude_spot.xls
# Output: gas_clean_balanced.csv (core panel); Brent_MA3 computed with 63-day window
# Note: Q4 2015 provides burn-in for moving average; estimation sample 2016-01-04 onwards


pkgs <- c("readxl", "dplyr", "tidyr", "readr", "here", "zoo", "tibble")

to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

project_dir <- here::here()

model_output_dir <- file.path(project_dir, "Model", "model output")
clean_output_dir <- file.path(model_output_dir, "Data Cleaning")

if (!dir.exists(clean_output_dir)) {
  dir.create(clean_output_dir, recursive = TRUE)
}

gas_file <- file.path(project_dir, "Gas prices.xlsx")
brent_spot_file <- file.path(project_dir, "brent_crude_spot.xls")

if (!file.exists(gas_file)) stop("Gas file not found: ", gas_file)
if (!file.exists(brent_spot_file)) stop("Brent spot file not found: ", brent_spot_file)

gas_hubs <- c("JKM", "NBP", "TTF", "HH")

BBL_TO_MMBTU <- 5.8
MA3_WINDOW <- 63L

MODEL_START <- as.Date("2016-01-04")
MODEL_END   <- as.Date("2026-03-31")

SPOT_START <- as.Date("2015-10-01")
SPOT_END   <- as.Date("2026-03-31")

core_series <- c(gas_hubs, "Brent_crude")
ma3_series  <- c(gas_hubs, "Brent_crude", "Brent_MA3")


# --- Helper functions ---

as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)

  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  parsed <- suppressWarnings(as.Date(x))

  if (all(is.na(parsed)) && any(!is.na(x))) {
    parsed <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  }

  if (all(is.na(parsed)) && any(!is.na(x))) {
    parsed <- suppressWarnings(as.Date(x, format = "%d.%m.%Y"))
  }

  if (all(is.na(parsed)) && any(!is.na(x))) {
    parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  }

  parsed
}

check_no_duplicates <- function(df, label) {
  if (any(duplicated(df$Date))) {
    stop(
      "Duplicate dates found in ",
      label,
      ". Joins would silently multiply rows. Fix source data."
    )
  }
}

check_positive <- function(df, col, label) {
  vals <- df[[col]][!is.na(df[[col]])]

  if (any(vals <= 0)) {
    stop(
      sprintf(
        "Non-positive values found in %s during %s: %d values <= 0.",
        col,
        label,
        sum(vals <= 0)
      )
    )
  }
}


# Load gas prices

gas_raw <- read_excel(
  gas_file,
  range = cell_cols("A:E"),
  na = c("", "NA")
)

expected_gas_cols <- c("Date", "JKM", "NBP", "TTF", "HH")

if (!all(expected_gas_cols %in% names(gas_raw))) {
  stop(
    "Gas workbook schema mismatch.\n",
    "Expected: ", paste(expected_gas_cols, collapse = ", "), "\n",
    "Found:    ", paste(names(gas_raw), collapse = ", ")
  )
}

gas_raw_filtered <- gas_raw |>
  filter(!is.na(Date))

gas_raw_numeric <- gas_raw_filtered |>
  mutate(across(all_of(gas_hubs), ~ as.numeric(as.character(.x))))

coercion_na_gas <- sapply(gas_hubs, function(h) {
  sum(is.na(gas_raw_numeric[[h]]) & !is.na(gas_raw_filtered[[h]]))
})

if (any(coercion_na_gas > 0)) {
  warning(
    "Numeric coercion introduced new NAs in gas hubs:\n",
    paste(
      sprintf(
        "%s: %d new NAs",
        names(coercion_na_gas[coercion_na_gas > 0]),
        coercion_na_gas[coercion_na_gas > 0]
      ),
      collapse = "\n"
    )
  )
}

gas <- gas_raw_numeric |>
  mutate(Date = as_date_safe(Date)) |>
  filter(Date >= MODEL_START, Date <= MODEL_END) |>
  arrange(Date)

rm(gas_raw_filtered, gas_raw_numeric)

check_no_duplicates(gas, "gas file")

for (h in gas_hubs) {
  check_positive(gas, h, "gas validation")
}


# Load Brent crude spot prices and compute 63-day moving average
# EIA workbook: skip = 2 puts row 3 (date + series name) as the header

spot_raw <- read_excel(
  brent_spot_file,
  sheet = "Data 1",
  skip = 2,
  na = c("", "NA", ".")
)

if (ncol(spot_raw) < 2) {
  stop("Brent spot file does not contain at least two columns on sheet 'Data 1'.")
}

spot_raw <- spot_raw |>
  select(1, 2)

names(spot_raw) <- c("Date", "Brent_spot")

spot_raw_filtered <- spot_raw |>
  filter(!is.na(Date))

spot_raw_numeric <- spot_raw_filtered |>
  mutate(
    Date = as_date_safe(Date),
    Brent_spot = as.numeric(as.character(Brent_spot))
  )

coercion_na_spot <- sum(
  is.na(spot_raw_numeric$Brent_spot) &
    !is.na(spot_raw_filtered$Brent_spot)
)

if (coercion_na_spot > 0) {
  warning("Numeric coercion introduced ", coercion_na_spot, " new NAs in Brent spot.")
}

spot <- spot_raw_numeric |>
  filter(Date >= SPOT_START, Date <= SPOT_END) |>
  arrange(Date) |>
  select(Date, Brent_spot)

rm(spot_raw_filtered, spot_raw_numeric)

check_no_duplicates(spot, "Brent spot file")
check_positive(spot, "Brent_spot", "pre-conversion Brent spot validation")

brent_spot_mean_bbl <- mean(spot$Brent_spot, na.rm = TRUE)

spot <- spot |>
  mutate(
    Brent_crude = Brent_spot / BBL_TO_MMBTU,
    Brent_MA3 = zoo::rollmean(
      Brent_crude,
      k = MA3_WINDOW,
      fill = NA,
      align = "right"
    )
  ) |>
  select(Date, Brent_crude, Brent_MA3)

brent_spot_mean_mmbtu <- mean(spot$Brent_crude, na.rm = TRUE)

check_positive(spot, "Brent_crude", "post-conversion Brent spot validation")

spot_ma3_nonmissing <- spot |>
  filter(!is.na(Brent_MA3))

if (nrow(spot_ma3_nonmissing) == 0) {
  stop("Brent_MA3 is entirely missing. Check Brent spot input and MA3 window.")
}

check_positive(spot_ma3_nonmissing, "Brent_MA3", "Brent_MA3 validation")

first_spot_date <- min(spot$Date, na.rm = TRUE)
last_spot_date  <- max(spot$Date, na.rm = TRUE)
first_ma3_date  <- min(spot$Date[!is.na(spot$Brent_MA3)], na.rm = TRUE)

spot_model <- spot |>
  filter(Date >= MODEL_START, Date <= MODEL_END)


# Join gas and Brent, check date overlap

gas_dates  <- unique(gas$Date)
spot_dates <- unique(spot_model$Date)

gas_only  <- setdiff(gas_dates, spot_dates)
spot_only <- setdiff(spot_dates, gas_dates)

combined <- gas |>
  inner_join(spot_model, by = "Date") |>
  arrange(Date)

if (nrow(combined) == 0) {
  stop("Combined panel has zero rows after joining gas and Brent spot files.")
}

for (col in core_series) {
  check_positive(combined, col, "combined panel validation")
}


# Build balanced panels
# Core: all four hubs + Brent_crude observed
# MA3:  additionally requires Brent_MA3 (excludes the 63-day burn-in period)

balanced <- combined |>
  filter(if_all(all_of(core_series), ~ !is.na(.x)))

balanced_ma3 <- combined |>
  filter(if_all(all_of(ma3_series), ~ !is.na(.x)))

remaining_na <- sum(is.na(balanced[core_series]))
remaining_na_ma3 <- sum(is.na(balanced_ma3[ma3_series]))

if (remaining_na > 0) {
  stop("Balanced panel still contains ", remaining_na, " missing values.")
}

if (remaining_na_ma3 > 0) {
  stop("MA3 balanced panel still contains ", remaining_na_ma3, " missing values.")
}

if (nrow(balanced_ma3) == 0) {
  stop("MA3 balanced panel has zero rows. Check Brent_MA3 construction.")
}


## Missingness diagnostics

n_raw_aligned <- nrow(combined)
n_balanced <- nrow(balanced)
n_balanced_ma3 <- nrow(balanced_ma3)

n_dropped_core <- n_raw_aligned - n_balanced
n_dropped_ma3  <- n_raw_aligned - n_balanced_ma3

miss_by_hub <- data.frame(
  Hub = gas_hubs,
  Missing = sapply(gas_hubs, function(h) sum(is.na(combined[[h]]))),
  stringsAsFactors = FALSE
)

incomplete_mask <- !complete.cases(combined[core_series])
incomplete_rows <- combined[incomplete_mask, ]

missing_long <- incomplete_rows |>
  select(Date, all_of(gas_hubs)) |>
  pivot_longer(
    cols = all_of(gas_hubs),
    names_to = "Hub",
    values_to = "Price"
  ) |>
  filter(is.na(Price)) |>
  select(Date, Hub) |>
  arrange(Date, Hub)

miss_by_year <- missing_long |>
  mutate(Year = as.integer(format(Date, "%Y"))) |>
  group_by(Hub, Year) |>
  summarise(Missing = n(), .groups = "drop") |>
  arrange(Hub, Year)

miss_by_month <- missing_long |>
  mutate(Month = as.integer(format(Date, "%m"))) |>
  group_by(Hub, Month) |>
  summarise(Missing = n(), .groups = "drop") |>
  arrange(Hub, Month)

pair_grid <- combn(gas_hubs, 2, simplify = FALSE)

pairwise_loss <- do.call(rbind, lapply(pair_grid, function(pair) {
  avail <- sum(complete.cases(combined[pair]))

  data.frame(
    Hub1 = pair[1],
    Hub2 = pair[2],
    Available = avail,
    Balanced = n_balanced,
    Balanced_MA3 = n_balanced_ma3,
    Lost_core = avail - n_balanced,
    Lost_MA3 = avail - n_balanced_ma3,
    Lost_core_pct = round(100 * (avail - n_balanced) / max(avail, 1), 2),
    Lost_MA3_pct = round(100 * (avail - n_balanced_ma3) / max(avail, 1), 2),
    stringsAsFactors = FALSE
  )
}))

directed_pairs <- expand.grid(
  Dependent = gas_hubs,
  Independent = gas_hubs,
  stringsAsFactors = FALSE
) |>
  filter(Dependent != Independent)

directed_sample_loss <- directed_pairs |>
  rowwise() |>
  mutate(
    Available = sum(complete.cases(combined[c(Dependent, Independent)])),
    Balanced = n_balanced,
    Balanced_MA3 = n_balanced_ma3,
    Lost_core = Available - Balanced,
    Lost_MA3 = Available - Balanced_MA3,
    Lost_core_pct = round(100 * Lost_core / max(Available, 1), 2),
    Lost_MA3_pct = round(100 * Lost_MA3 / max(Available, 1), 2)
  ) |>
  ungroup()

stress_windows <- data.frame(
  Window = c(
    "COVID (2020-02 to 2020-06)",
    "Energy crisis (2021-09 to 2022-12)",
    "Post-invasion (2022-02 to 2022-12)",
    "Normalization (2023-01 to 2024-12)"
  ),
  Start = as.Date(c(
    "2020-02-01",
    "2021-09-01",
    "2022-02-24",
    "2023-01-01"
  )),
  End = as.Date(c(
    "2020-06-30",
    "2022-12-31",
    "2022-12-31",
    "2024-12-31"
  )),
  stringsAsFactors = FALSE
)

stress_miss <- do.call(rbind, lapply(seq_len(nrow(stress_windows)), function(i) {
  w <- stress_windows[i, ]

  mask <- combined$Date >= w$Start & combined$Date <= w$End
  sub <- combined[mask, ]

  inc_core <- sum(!complete.cases(sub[core_series]))
  inc_ma3  <- sum(!complete.cases(sub[ma3_series]))

  data.frame(
    Window = w$Window,
    Total_dates = nrow(sub),
    Incomplete_core_rows = inc_core,
    Incomplete_core_pct = round(100 * inc_core / max(nrow(sub), 1), 2),
    Incomplete_MA3_rows = inc_ma3,
    Incomplete_MA3_pct = round(100 * inc_ma3 / max(nrow(sub), 1), 2),
    stringsAsFactors = FALSE
  )
}))

stress_miss_by_hub <- do.call(rbind, lapply(seq_len(nrow(stress_windows)), function(i) {
  w <- stress_windows[i, ]

  sub <- missing_long |>
    filter(Date >= w$Start, Date <= w$End)

  if (nrow(sub) == 0) {
    data.frame(
      Window = w$Window,
      Hub = NA_character_,
      Missing = 0L,
      stringsAsFactors = FALSE
    )
  } else {
    sub |>
      count(Hub, name = "Missing") |>
      mutate(Window = w$Window, .before = 1) |>
      as.data.frame(stringsAsFactors = FALSE)
  }
})) |>
  filter(!is.na(Hub))


# Write diagnostic CSVs

write_csv(miss_by_hub, file.path(clean_output_dir, "missingness_by_hub.csv"))
write_csv(miss_by_year, file.path(clean_output_dir, "missingness_by_year.csv"))
write_csv(miss_by_month, file.path(clean_output_dir, "missingness_by_month.csv"))
write_csv(missing_long, file.path(clean_output_dir, "missing_dates_long.csv"))
write_csv(pairwise_loss, file.path(clean_output_dir, "pairwise_sample_loss.csv"))
write_csv(directed_sample_loss, file.path(clean_output_dir, "directed_sample_loss.csv"))
write_csv(stress_miss, file.path(clean_output_dir, "missingness_stress_windows.csv"))
write_csv(stress_miss_by_hub, file.path(clean_output_dir, "missingness_stress_windows_by_hub.csv"))

write_csv(
  data.frame(Date = sort(gas_only), stringsAsFactors = FALSE),
  file.path(clean_output_dir, "gas_only_dates.csv")
)

write_csv(
  data.frame(Date = sort(spot_only), stringsAsFactors = FALSE),
  file.path(clean_output_dir, "spot_only_dates.csv")
)


# Cleaning log

na_counts <- colSums(is.na(combined))

log_lines <- c(
  paste("Data Cleaning Log —", Sys.time()),
  "",
  "--- Source files ---",
  sprintf("  Gas rows loaded:             %d", nrow(gas)),
  sprintf("  Brent spot rows loaded:      %d", nrow(spot)),
  "",
  "--- Model sample contract ---",
  sprintf("  Model start:                 %s", MODEL_START),
  sprintf("  Model end:                   %s", MODEL_END),
  sprintf("  Brent spot input start:      %s", SPOT_START),
  sprintf("  Brent spot input end:        %s", SPOT_END),
  sprintf("  Brent_MA3 window:            %d business days", MA3_WINDOW),
  sprintf("  First Brent_MA3 date:        %s", first_ma3_date),
  "",
  "--- Join diagnostics ---",
  sprintf("  Gas-only dates:              %d", length(gas_only)),
  sprintf("  Spot-only dates:             %d", length(spot_only)),
  sprintf("  Common gas-spot dates:       %d", n_raw_aligned),
  "",
  "--- Brent conversion ---",
  sprintf("  Conversion factor:           %.2f mmBtu/bbl", BBL_TO_MMBTU),
  sprintf("  Spot Brent mean USD/bbl:     %.3f", brent_spot_mean_bbl),
  sprintf("  Spot Brent mean USD/mmBtu:   %.3f", brent_spot_mean_mmbtu),
  "",
  "--- Integrity checks ---",
  sprintf("  Duplicate dates in gas file: %s", ifelse(any(duplicated(gas$Date)), "FAIL", "PASS")),
  sprintf("  Duplicate dates in spot file:%s", ifelse(any(duplicated(spot$Date)), "FAIL", "PASS")),
  sprintf("  Gas coercion NAs introduced: %d", sum(pmax(coercion_na_gas, 0))),
  sprintf("  Spot coercion NAs introduced:%d", max(coercion_na_spot, 0)),
  "",
  "--- Output sample sizes ---",
  sprintf("  Raw aligned panel:           %d", n_raw_aligned),
  sprintf("  Core balanced panel:         %d", n_balanced),
  sprintf("  MA3 balanced panel:          %d", n_balanced_ma3),
  sprintf("  Dropped for core panel:      %d", n_dropped_core),
  sprintf("  Dropped for MA3 panel:       %d", n_dropped_ma3),
  sprintf("  Missing values in core:      %d", remaining_na),
  sprintf("  Missing values in MA3:       %d", remaining_na_ma3),
  "",
  "--- Missing values per column in raw aligned panel ---"
)

for (col in names(na_counts)) {
  log_lines <- c(
    log_lines,
    sprintf(
      "  %-14s %d (%.1f%%)",
      col,
      na_counts[col],
      100 * na_counts[col] / n_raw_aligned
    )
  )
}

log_lines <- c(
  log_lines,
  "",
  "--- Pairwise sample loss ---"
)

for (i in seq_len(nrow(pairwise_loss))) {
  r <- pairwise_loss[i, ]

  log_lines <- c(
    log_lines,
    sprintf(
      "  %s-%s: available=%d, core=%d, MA3=%d, lost_core=%d, lost_MA3=%d",
      r$Hub1,
      r$Hub2,
      r$Available,
      r$Balanced,
      r$Balanced_MA3,
      r$Lost_core,
      r$Lost_MA3
    )
  )
}

log_lines <- c(
  log_lines,
  "",
  "--- Output files ---",
  "  gas_clean_raw_aligned.csv       audit panel, all common gas-spot dates",
  "  gas_clean_balanced.csv          core estimation panel",
  "  gas_clean_ma3_balanced.csv      Brent spot moving-average estimation panel",
  "  gas_clean.csv                   alias of gas_clean_balanced.csv",
  "",
  "--- Downstream rule ---",
  "  Standard no-oil Kalman scripts should use gas_clean_balanced.csv.",
  "  Oil and dummy Kalman scripts using Brent_MA3 should use gas_clean_ma3_balanced.csv.",
  "  Brent_crude and Brent_MA3 are both based on brent_crude_spot.xls."
)

writeLines(log_lines, file.path(clean_output_dir, "cleaning_log.txt"))
cat(paste(log_lines, collapse = "\n"), "\n")


# Save output files

write_csv(combined, file.path(clean_output_dir, "gas_clean_raw_aligned.csv"))
write_csv(balanced, file.path(clean_output_dir, "gas_clean_balanced.csv"))
write_csv(balanced_ma3, file.path(clean_output_dir, "gas_clean_ma3_balanced.csv"))

write_csv(balanced, file.path(clean_output_dir, "gas_clean.csv"))

cat("\n== Finished Data Cleaning ==\n")
cat(sprintf("   Raw aligned panel:  %d obs -> gas_clean_raw_aligned.csv\n", n_raw_aligned))
cat(sprintf("   Core balanced:      %d obs -> gas_clean_balanced.csv\n", n_balanced))
cat(sprintf("   MA3 balanced:       %d obs -> gas_clean_ma3_balanced.csv\n", n_balanced_ma3))
cat("   Alias:              gas_clean.csv -> gas_clean_balanced.csv\n")
cat("   Brent spot MA3 scripts should now read gas_clean_ma3_balanced.csv\n")