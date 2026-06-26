# ============================================================
# dependencies.R — Project package setup via renv
#
# Run via:
#   python run.py setup
#
# First run: initialises a project-local renv library, installs
#   all packages, and writes renv.lock.
# Subsequent runs: restores the library from renv.lock.
# ============================================================

options(renv.consent = TRUE)   # suppress interactive consent prompts

# 1. Bootstrap renv itself into a writable library if not present
if (!requireNamespace("renv", quietly = TRUE)) {
  message("renv not found — locating a writable R library...")

  # Find the first writable entry on .libPaths(), or create a user library
  writable <- .libPaths()[file.access(.libPaths(), mode = 2) == 0]
  if (length(writable) == 0) {
    ver      <- paste0(R.version$major, ".", substr(R.version$minor, 1, 1))
    app_data <- Sys.getenv("LOCALAPPDATA",
                  unset = Sys.getenv("APPDATA", unset = path.expand("~")))
    user_lib <- file.path(app_data, "R", "win-library", ver)
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(user_lib, .libPaths()))
    writable <- user_lib
    message(sprintf("Created user library: %s", user_lib))
  }

  message("Installing renv...")
  install.packages("renv", repos = "https://cloud.r-project.org", lib = writable[1])
  library(renv, lib.loc = writable[1])
}

# 2. Full list of project packages
pkgs <- c(
  "dlm",        # Kalman filter / dynamic linear models
  "dplyr",      # Data manipulation
  "FinTS",      # ARCH-LM test (Descriptive Analysis)
  "forcats",    # Factor helpers (Visualization)
  "ggplot2",    # Plotting
  "gt",         # Publication-quality tables
  "here",       # Project-relative paths (here::here())
  "lmtest",     # Granger causality tests
  "moments",    # Skewness / kurtosis
  "patchwork",  # Combining ggplot figures
  "purrr",      # Functional programming helpers
  "readr",      # CSV I/O
  "readxl",     # Excel I/O (.xlsx, .xls)
  "scales",     # Axis formatting in ggplot2
  "strucchange",# Bai-Perron structural break tests
  "stringr",    # String manipulation
  "tibble",     # Modern data frames
  "tidyr",      # Data reshaping
  "tseries",    # ADF / unit root tests
  "urca",       # PP / KPSS / Zivot-Andrews tests
  "vars",       # VAR models (Granger causality)
  "webshot2",   # Save gt tables as PNG images (requires Chrome)
  "zoo"         # Rolling window functions (rollmean)
)

# 3. First-time init vs. install/update from pkgs list
if (!file.exists("renv.lock")) {
  message("First-time setup: initialising renv project library...")
  renv::init(bare = TRUE, restart = FALSE)
}

message("Installing / updating packages — this may take several minutes...")
renv::install(pkgs)

message("Writing renv.lock...")
renv::snapshot(prompt = FALSE)

message("\nDone.")
