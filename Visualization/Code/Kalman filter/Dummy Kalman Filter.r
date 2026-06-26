# Dummy Kalman Filter - Visualisation
# Input: State paths, MLE parameters, integration tests
# Output: Publication-ready plots and tables; no embedded titles

pkgs <- c(
  "dplyr", "readr", "tidyr", "tibble",
  "ggplot2", "gt", "scales", "patchwork", "here"
)

to_install <- pkgs[!pkgs %in% installed.packages()[, 1]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

project_dir <- here::here()

kf_model_dir <- file.path(
  project_dir, "Model", "model output",
  "Kalman filter", "Dummy Kalman Filter"
)

comp_dir <- file.path(
  project_dir, "Model", "model output",
  "Model Comparison"
)

break_file <- file.path(
  project_dir, "Model", "model output",
  "Unit Root and Structural Break",
  "Pairwise Spreads", "spread_BaiPerron_breaks.csv"
)

kf_vis_dir <- file.path(
  project_dir, "Visualization", "Plot outputs",
  "Kalman filter", "Dummy Kalman Filter"
)

plots_dir  <- file.path(kf_vis_dir, "Plots")
tables_dir <- file.path(kf_vis_dir, "Tables")
resid_dir  <- file.path(kf_vis_dir, "Residuals")

invisible(lapply(
  c(plots_dir, tables_dir, resid_dir),
  function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
))

# File checks
required_files <- c(
  file.path(kf_model_dir, "state_paths.csv"),
  file.path(kf_model_dir, "standardised_residuals.csv"),
  file.path(kf_model_dir, "mle_parameters.csv"),
  file.path(kf_model_dir, "integration_tests.csv"),
  file.path(kf_model_dir, "model_diagnostics.csv")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required model output files:\n",
    paste(missing_files, collapse = "\n")
  )
}

# Load data

state_paths <- read_csv(
  file.path(kf_model_dir, "state_paths.csv"),
  show_col_types = FALSE
) |>
  mutate(Date = as.Date(Date))

residuals_df <- read_csv(
  file.path(kf_model_dir, "standardised_residuals.csv"),
  show_col_types = FALSE
) |>
  mutate(Date = as.Date(Date))

mle_pars <- read_csv(
  file.path(kf_model_dir, "mle_parameters.csv"),
  show_col_types = FALSE
)

integ_tests <- read_csv(
  file.path(kf_model_dir, "integration_tests.csv"),
  show_col_types = FALSE
)

diag_df <- read_csv(
  file.path(kf_model_dir, "model_diagnostics.csv"),
  show_col_types = FALSE
)

pair_summary_file <- file.path(kf_model_dir, "integration_summary_by_pair.csv")
pair_summary <- NULL
if (file.exists(pair_summary_file)) {
  pair_summary <- read_csv(pair_summary_file, show_col_types = FALSE)
}

rolling_ols <- NULL
ols_file <- file.path(comp_dir, "rolling_ols_betas.csv")
if (file.exists(ols_file)) {
  rolling_ols <- read_csv(ols_file, show_col_types = FALSE) |>
    mutate(Date = as.Date(Date))
}

break_df <- NULL
if (file.exists(break_file)) {
  break_df <- read_csv(break_file, show_col_types = FALSE) |>
    mutate(Break_date = as.Date(Break_date))

  if ("Type" %in% names(break_df)) {
    break_df <- break_df |>
      filter(Type == "Gas-Gas")
  }
}

# Required column checks

required_state_cols <- c(
  "Dependent", "Independent", "Date",
  "Alpha", "Alpha_lower", "Alpha_upper",
  "Beta_base", "Beta_base_lower", "Beta_base_upper",
  "Beta_eff", "Beta_eff_lower", "Beta_eff_upper",
  "AR_Denominator",
  "Delta", "Delta_lower", "Delta_upper",
  "Phi1", "Phi2"
)

missing_state_cols <- setdiff(required_state_cols, names(state_paths))

if (length(missing_state_cols) > 0) {
  stop(
    "state_paths.csv is missing required columns:\n",
    paste(missing_state_cols, collapse = ", ")
  )
}

required_integ_cols <- c(
  "Dependent", "Independent",
  "Beta_eff_impact_mean_full",
  "Beta_eff_impact_mean_terminal",
  "Beta_eff_impact_pct_CI_1",
  "Impact_convergence_shift",
  "Delta_impact_mean_full",
  "Delta_impact_mean_terminal",
  "AR_Denominator",
  "AR_Stable",
  "AR_Denom_Caution",
  "Corr_gas_oil_regressors"
)

missing_integ_cols <- setdiff(required_integ_cols, names(integ_tests))

if (length(missing_integ_cols) > 0) {
  stop(
    "integration_tests.csv is missing required columns:\n",
    paste(missing_integ_cols, collapse = ", ")
  )
}

# Pair and break helpers

pairs <- state_paths |>
  distinct(Dependent, Independent) |>
  arrange(Dependent, Independent)

get_breaks <- function(dep, ind) {
  if (is.null(break_df)) return(as.Date(character(0)))

  break_df |>
    filter(
      (Hub_A == dep & Hub_B == ind) |
        (Hub_A == ind & Hub_B == dep)
    ) |>
    arrange(Break_date) |>
    pull(Break_date) |>
    unique()
}

## Visual identity

hub_colors <- c(
  "JKM"         = "#1B1B3A",
  "TTF"         = "#1F78B4",
  "NBP"         = "#33A02C",
  "HH"          = "#FF7F00",
  "Brent_crude" = "#B22182"
)

hub_labels <- c(
  "JKM"         = "JKM",
  "TTF"         = "TTF",
  "NBP"         = "NBP",
  "HH"          = "Henry Hub",
  "Brent_crude" = "Brent crude"
)

state_colors <- c(
  "alpha"    = "#33A02C",
  "beta"     = "#1B1B3A",
  "betabase" = "#6A6A8A",
  "delta"    = "#B22182",
  "omega"    = "#8A4F7D",
  "phi1"     = "#8B6914",
  "phi2"     = "#6B4226"
)

theme_latex_compact <- function(base_size = 8.5) {
  theme_bw(base_family = "serif", base_size = base_size) +
    theme(
      text             = element_text(color = "black"),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = 7, color = "black"),
      axis.line        = element_line(color = "black", linewidth = 0.25),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.3),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
      strip.text       = element_text(size = 8),
      legend.position  = "none",
      plot.margin      = margin(3, 4, 3, 3)
    )
}

# Backward compatibility alias
theme_thesis <- theme_latex_compact

# Helper function for saving LaTeX-compatible plots
save_latex_plot <- function(plot_obj, filepath, width = 7.2, height = 5.2, dpi = 300) {
  ggsave(
    filepath,
    plot_obj,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}

format_academic_table <- function(gt_obj) {
  gt_obj |>
    tab_options(
      table.font.names                  = "Times New Roman",
      table.font.size                   = 13,
      heading.align                     = "left",
      table.border.top.style            = "solid",
      table.border.top.width            = px(2),
      table.border.top.color            = "black",
      table.border.bottom.style         = "solid",
      table.border.bottom.width         = px(2),
      table.border.bottom.color         = "black",
      column_labels.border.top.width    = px(0),
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      column_labels.font.weight         = "bold",
      table_body.hlines.width           = px(0),
      table_body.vlines.width           = px(0),
      data_row.padding                  = px(4),
      source_notes.font.size            = 11,
      source_notes.padding              = px(6)
    ) |>
    cols_align(align = "right", columns = where(is.numeric)) |>
    opt_table_font(font = list("Times New Roman", "Times", "serif"))
}

add_stars <- function(p) {
  case_when(
    is.na(p)  ~ "",
    p < 0.01  ~ "***",
    p < 0.05  ~ "**",
    p < 0.10  ~ "*",
    TRUE      ~ ""
  )
}

save_gt_safe <- function(gt_obj, filename) {
  tryCatch(
    gt::gtsave(gt_obj, filename),
    error = function(e) {
      warning(
        "Could not save table image: ", filename, "\n",
        "Reason: ", conditionMessage(e), "\n",
        "The gt table object was created, but image export failed. ",
        "This is usually a webshot2/chromote issue."
      )
    }
  )
}

# Plot labels

ylab_alpha <- expression(alpha[t])
ylab_beta_eff <- expression(beta[t]^eff)
ylab_beta_base <- expression(beta[t]^base)
ylab_delta <- expression(delta[t])
ylab_resid <- expression(tilde(v)[t])

# 1. State paths per pair

cat("Generating state path plots ...\n")

for (i in seq_len(nrow(pairs))) {
  dep <- pairs$Dependent[i]
  ind <- pairs$Independent[i]

  df <- state_paths |>
    filter(Dependent == dep, Independent == ind)

  breaks <- get_breaks(dep, ind)

  add_break_lines <- function() {
    if (length(breaks) > 0) {
      geom_vline(
        xintercept = breaks,
        linetype = "dashed",
        color = "grey55",
        linewidth = 0.3,
        alpha = 0.6
      )
    }
  }

  p_a <- ggplot(df, aes(x = Date)) +
    geom_ribbon(
      aes(ymin = Alpha_lower, ymax = Alpha_upper),
      fill = state_colors["alpha"],
      alpha = 0.18
    ) +
    geom_line(aes(y = Alpha), color = state_colors["alpha"], linewidth = 0.65) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.35) +
    add_break_lines() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(y = ylab_alpha, x = NULL) +
    theme_thesis()

  p_b <- ggplot(df, aes(x = Date)) +
    geom_ribbon(
      aes(ymin = Beta_base_lower, ymax = Beta_base_upper),
      fill = state_colors["betabase"],
      alpha = 0.15
    ) +
    geom_line(
      aes(y = Beta_base),
      color = state_colors["betabase"],
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    geom_ribbon(
      aes(ymin = Beta_eff_lower, ymax = Beta_eff_upper),
      fill = state_colors["beta"],
      alpha = 0.22
    ) +
    geom_line(aes(y = Beta_eff), color = state_colors["beta"], linewidth = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.35) +
    add_break_lines() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(y = ylab_beta_eff, x = NULL) +
    theme_thesis()

  p_d <- ggplot(df, aes(x = Date)) +
    geom_ribbon(
      aes(ymin = Delta_lower, ymax = Delta_upper),
      fill = state_colors["delta"],
      alpha = 0.18
    ) +
    geom_line(aes(y = Delta), color = state_colors["delta"], linewidth = 0.65) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.35) +
    add_break_lines() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(y = ylab_delta, x = NULL) +
    theme_thesis()

  p_all <- p_a / p_b / p_d

  ggsave(
    file.path(plots_dir, paste0("States_", dep, "_", ind, ".png")),
    p_all,
    width = 7.2,
    height = 8.0,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

# 2. Per-pair beta and delta plots

cat("Generating single-state pair plots ...\n")

for (i in seq_len(nrow(pairs))) {
  dep <- pairs$Dependent[i]
  ind <- pairs$Independent[i]

  df <- state_paths |>
    filter(Dependent == dep, Independent == ind)

  breaks <- get_breaks(dep, ind)

  break_layer <- if (length(breaks) > 0) {
    geom_vline(
      xintercept = breaks,
      linetype = "dashed",
      color = "grey55",
      linewidth = 0.3,
      alpha = 0.6
    )
  }

  p_beta <- ggplot(df, aes(x = Date)) +
    geom_ribbon(
      aes(ymin = Beta_base_lower, ymax = Beta_base_upper),
      fill = state_colors["betabase"],
      alpha = 0.15
    ) +
    geom_line(
      aes(y = Beta_base),
      color = state_colors["betabase"],
      linewidth = 0.45,
      linetype = "dashed"
    ) +
    geom_ribbon(
      aes(ymin = Beta_eff_lower, ymax = Beta_eff_upper),
      fill = state_colors["beta"],
      alpha = 0.22
    ) +
    geom_line(aes(y = Beta_eff), color = state_colors["beta"], linewidth = 0.75) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.35) +
    break_layer +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(x = NULL, y = ylab_beta_eff) +
    theme_thesis()

  ggsave(
    file.path(plots_dir, paste0("Beta_", dep, "_", ind, ".png")),
    p_beta,
    width = 7.2,
    height = 4.0,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  p_delta <- ggplot(df, aes(x = Date)) +
    geom_ribbon(
      aes(ymin = Delta_lower, ymax = Delta_upper),
      fill = state_colors["delta"],
      alpha = 0.22
    ) +
    geom_line(aes(y = Delta), color = state_colors["delta"], linewidth = 0.75) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.35) +
    break_layer +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(x = NULL, y = ylab_delta) +
    theme_thesis()

  ggsave(
    file.path(plots_dir, paste0("Delta_", dep, "_", ind, ".png")),
    p_delta,
    width = 7.2,
    height = 4.0,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

# 3. Mega plots

cat("Generating mega plots ...\n")

pair_order_codes <- tribble(
  ~Dependent, ~Independent,
  "JKM", "TTF",
  "TTF", "JKM",
  "NBP", "JKM",
  "HH",  "JKM",
  "JKM", "NBP",
  "TTF", "NBP",
  "NBP", "TTF",
  "HH",  "TTF",
  "JKM", "HH",
  "TTF", "HH",
  "NBP", "HH",
  "HH",  "NBP"
) |>
  mutate(
    Pair_code = paste0(Dependent, " ~ ", Independent),
    Pair = paste0(hub_labels[Dependent], " ~ ", hub_labels[Independent])
  )

have_pairs <- state_paths |>
  distinct(Dependent, Independent) |>
  mutate(Pair_code = paste0(Dependent, " ~ ", Independent)) |>
  pull(Pair_code)

missing_pairs <- setdiff(pair_order_codes$Pair_code, have_pairs)
if (length(missing_pairs) > 0) {
  warning("Missing pairs in state_paths: ", paste(missing_pairs, collapse = "; "))
}

extra_pairs <- setdiff(have_pairs, pair_order_codes$Pair_code)
if (length(extra_pairs) > 0) {
  warning(
    "Pairs in state_paths not in pair_order: ",
    paste(extra_pairs, collapse = "; "),
    ". They will be excluded from mega plots."
  )
}

all_sp <- state_paths |>
  mutate(Pair_code = paste0(Dependent, " ~ ", Independent)) |>
  inner_join(
    pair_order_codes |>
      dplyr::select(Pair_code, Pair),
    by = "Pair_code"
  ) |>
  mutate(Pair = factor(Pair, levels = pair_order_codes$Pair))

beta_y_lims <- c(-0.3, 1.25)

p_mega_b <- ggplot(all_sp, aes(x = Date)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey70", linewidth = 0.3) +
  geom_ribbon(
    aes(ymin = Beta_eff_lower, ymax = Beta_eff_upper),
    fill = state_colors["beta"],
    alpha = 0.22
  ) +
  geom_line(aes(y = Beta_eff), color = state_colors["beta"], linewidth = 0.45) +
  facet_wrap(~ Pair, ncol = 4, scales = "fixed") +
  coord_cartesian(ylim = beta_y_lims) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = ylab_beta_eff) +
  theme_thesis() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.3, "lines")
  )

save_latex_plot(p_mega_b, file.path(plots_dir, "Beta_all_pairs.png"),
                 width = 7.2, height = 5.4)

p_mega_bbase <- ggplot(all_sp, aes(x = Date)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey70", linewidth = 0.3) +
  geom_ribbon(
    aes(ymin = Beta_base_lower, ymax = Beta_base_upper),
    fill = state_colors["betabase"],
    alpha = 0.22
  ) +
  geom_line(aes(y = Beta_base), color = state_colors["betabase"], linewidth = 0.45) +
  facet_wrap(~ Pair, ncol = 4, scales = "fixed") +
  coord_cartesian(ylim = beta_y_lims) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = NULL, y = ylab_beta_base) +
  theme_thesis() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.3, "lines")
  )

save_latex_plot(p_mega_bbase, file.path(plots_dir, "Beta_base_all_pairs.png"),
                 width = 7.2, height = 5.4)

delta_range <- range(all_sp$Delta, na.rm = TRUE)
delta_pad <- max(abs(delta_range), na.rm = TRUE) * 1.05
if (!is.finite(delta_pad) || delta_pad == 0) delta_pad <- 1

p_mega_d <- ggplot(all_sp, aes(x = Date)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.4) +
  geom_ribbon(
    aes(ymin = Delta_lower, ymax = Delta_upper),
    fill = state_colors["delta"],
    alpha = 0.22
  ) +
  geom_line(aes(y = Delta), color = state_colors["delta"], linewidth = 0.45) +
  facet_wrap(~ Pair, ncol = 4, scales = "fixed") +
  coord_cartesian(ylim = c(-delta_pad, delta_pad)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(x = NULL, y = ylab_delta) +
  theme_thesis() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.3, "lines")
  )

save_latex_plot(p_mega_d, file.path(plots_dir, "Delta_all_pairs.png"),
                 width = 7.2, height = 5.4)

p_mega_a <- ggplot(all_sp, aes(x = Date)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.4) +
  geom_ribbon(
    aes(ymin = Alpha_lower, ymax = Alpha_upper),
    fill = state_colors["alpha"],
    alpha = 0.22
  ) +
  geom_line(aes(y = Alpha), color = state_colors["alpha"], linewidth = 0.45) +
  facet_wrap(~ Pair, ncol = 4, scales = "fixed") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(x = NULL, y = ylab_alpha) +
  theme_thesis() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.3, "lines")
  )

save_latex_plot(p_mega_a, file.path(plots_dir, "Alpha_all_pairs.png"),
                 width = 7.2, height = 5.4)

# 3B. Mega PACF plot

pcf_file <- file.path(kf_model_dir, "pacf_diagnostics.csv")

if (file.exists(pcf_file)) {
  cat("Generating mega PACF plot ...\n")

  pacf_diag <- read_csv(pcf_file, show_col_types = FALSE)

  pacf_for_plot <- pacf_diag |>
    mutate(Pair_code = paste0(Dependent, " ~ ", Independent)) |>
    inner_join(
      pair_order_codes |>
        dplyr::select(Pair_code, Pair),
      by = "Pair_code"
    ) |>
    mutate(Pair = factor(Pair, levels = pair_order_codes$Pair))

  ci_per_pair <- residuals_df |>
    group_by(Dependent, Independent) |>
    summarise(N = sum(is.finite(Residual_Std)), .groups = "drop") |>
    mutate(
      Pair_code = paste0(Dependent, " ~ ", Independent),
      CI_bound = 1.96 / sqrt(N)
    ) |>
    dplyr::select(Pair_code, CI_bound)

  pacf_for_plot <- pacf_for_plot |>
    left_join(ci_per_pair, by = "Pair_code")

  p_mega_pacf <- ggplot(pacf_for_plot, aes(x = Lag, y = PACF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    geom_col(width = 0.4, fill = state_colors["beta"]) +
    geom_hline(
      data = pacf_for_plot |> distinct(Pair, CI_bound),
      aes(yintercept = CI_bound),
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.35
    ) +
    geom_hline(
      data = pacf_for_plot |> distinct(Pair, CI_bound),
      aes(yintercept = -CI_bound),
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.35
    ) +
    facet_wrap(~ Pair, ncol = 4, scales = "fixed") +
    scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35)) +
    labs(x = "Lag", y = "PACF") +
    theme_thesis() +
    theme(
      strip.text = element_text(size = 8),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      panel.spacing = unit(0.3, "lines")
    )

  save_latex_plot(p_mega_pacf, file.path(plots_dir, "PACF_all_pairs.png"),
                   width = 7.2, height = 5.4)
} else {
  message("pacf_diagnostics.csv not found. Skipping mega PACF plot.")
}

# 4. OLS comparison

if (!is.null(rolling_ols)) {
  cat("Generating OLS comparison plots ...\n")

  for (i in seq_len(nrow(pairs))) {
    dep <- pairs$Dependent[i]
    ind <- pairs$Independent[i]

    kf_df <- state_paths |>
      filter(Dependent == dep, Independent == ind)

    roll <- rolling_ols |>
      filter(Dependent == dep, Independent == ind)

    if (!"Beta_OLS" %in% names(roll)) next

    roll <- roll |>
      dplyr::select(Date, Roll_Beta = Beta_OLS)

    plot_df <- left_join(kf_df, roll, by = "Date") |>
      filter(!is.na(Roll_Beta))

    if (nrow(plot_df) == 0) next

    p <- ggplot(plot_df, aes(x = Date)) +
      geom_hline(yintercept = 1, color = "grey50", linetype = "dashed", linewidth = 0.35) +
      geom_line(
        aes(y = Roll_Beta, color = "Rolling OLS (252-day)"),
        linewidth = 0.55,
        linetype = "dashed"
      ) +
      geom_line(aes(y = Beta_eff, color = "Kalman filter"), linewidth = 0.75) +
      scale_color_manual(
        name = NULL,
        values = c(
          "Rolling OLS (252-day)" = "#B22182",
          "Kalman filter" = state_colors["beta"]
        )
      ) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
      labs(x = NULL, y = ylab_beta_eff) +
      theme_thesis() +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm")
      )

    ggsave(
      file.path(plots_dir, paste0("OLS_comparison_", dep, "_", ind, ".png")),
      p,
      width = 7.2,
      height = 4.0,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }
}

# 5. Residuals

cat("Generating residual plots ...\n")

acf_list <- list()

for (i in seq_len(nrow(pairs))) {
  dep <- pairs$Dependent[i]
  ind <- pairs$Independent[i]

  res_sub <- residuals_df |>
    filter(Dependent == dep, Independent == ind) |>
    filter(is.finite(Residual_Std))

  if (nrow(res_sub) < 30) next

  p_ts <- ggplot(res_sub, aes(x = Date, y = Residual_Std)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "grey40", linewidth = 0.35) +
    geom_line(color = state_colors["beta"], linewidth = 0.35) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
    labs(x = NULL, y = ylab_resid) +
    theme_thesis()

  acf_obj <- acf(res_sub$Residual_Std, plot = FALSE, na.action = na.pass)

  acf_d <- tibble(
    Lag = as.numeric(acf_obj$lag[-1]),
    ACF = as.numeric(acf_obj$acf[-1])
  )

  ci <- 1.96 / sqrt(nrow(res_sub))

  acf_list[[paste0(dep, "_", ind)]] <- tibble(
    Dependent = dep,
    Independent = ind,
    Lag = as.integer(acf_d$Lag),
    ACF = acf_d$ACF,
    Significant = abs(acf_d$ACF) > ci
  )

  p_acf <- ggplot(acf_d, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "grey40", linewidth = 0.35) +
    geom_col(width = 0.4, fill = state_colors["beta"]) +
    labs(x = "Lag", y = "ACF") +
    theme_thesis()

  pacf_obj <- pacf(res_sub$Residual_Std, plot = FALSE, na.action = na.pass)

  pacf_d <- tibble(
    Lag = as.numeric(pacf_obj$lag[-1]),
    PACF = as.numeric(pacf_obj$acf[-1])
  )

  p_pacf <- ggplot(pacf_d, aes(x = Lag, y = PACF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "grey40", linewidth = 0.35) +
    geom_col(width = 0.4, fill = state_colors["beta"]) +
    labs(x = "Lag", y = "PACF") +
    theme_thesis()

  p_r <- p_ts / p_acf / p_pacf

  ggsave(
    file.path(resid_dir, paste0("Residuals_", dep, "_", ind, ".png")),
    p_r,
    width = 7.2,
    height = 6.0,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

if (length(acf_list) > 0) {
  write_csv(bind_rows(acf_list), file.path(kf_model_dir, "acf_diagnostics.csv"))
}

# 3C. Mega ACF plot

acf_file <- file.path(kf_model_dir, "acf_diagnostics.csv")

if (file.exists(acf_file)) {
  cat("Generating mega ACF plot ...\n")

  acf_diag <- read_csv(acf_file, show_col_types = FALSE)

  acf_for_plot <- acf_diag |>
    mutate(Pair_code = paste0(Dependent, " ~ ", Independent)) |>
    inner_join(
      pair_order_codes |>
        dplyr::select(Pair_code, Pair),
      by = "Pair_code"
    ) |>
    mutate(Pair = factor(Pair, levels = pair_order_codes$Pair))

  ci_per_pair_acf <- residuals_df |>
    group_by(Dependent, Independent) |>
    summarise(N = sum(is.finite(Residual_Std)), .groups = "drop") |>
    mutate(
      Pair_code = paste0(Dependent, " ~ ", Independent),
      CI_bound = 1.96 / sqrt(N)
    ) |>
    dplyr::select(Pair_code, CI_bound)

  acf_for_plot <- acf_for_plot |>
    left_join(ci_per_pair_acf, by = "Pair_code")

  p_mega_acf <- ggplot(acf_for_plot, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    geom_col(width = 0.4, fill = state_colors["beta"]) +
    geom_hline(
      data = acf_for_plot |> distinct(Pair, CI_bound),
      aes(yintercept = CI_bound),
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.35
    ) +
    geom_hline(
      data = acf_for_plot |> distinct(Pair, CI_bound),
      aes(yintercept = -CI_bound),
      linetype = "dashed",
      color = "grey40",
      linewidth = 0.35
    ) +
    facet_wrap(~ Pair, ncol = 4, scales = "fixed") +
    scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35)) +
    labs(x = "Lag", y = "ACF") +
    theme_thesis() +
    theme(
      strip.text = element_text(size = 8),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 7),
      panel.spacing = unit(0.3, "lines")
    )

  save_latex_plot(p_mega_acf, file.path(plots_dir, "ACF_all_pairs.png"),
                   width = 7.2, height = 5.4)
} else {
  message("acf_diagnostics.csv not found. Skipping mega ACF plot.")
}

# 6. Tables

cat("Generating tables ...\n")

# ------------------------------------------------------------------------------
# 6A. Integration tests
# ------------------------------------------------------------------------------

gt_int <- integ_tests |>
  mutate(
    Dependent = hub_labels[Dependent],
    Independent = hub_labels[Independent]
  ) |>
  dplyr::select(
    Dependent,
    Independent,
    Beta_eff_impact_mean_full,
    Beta_eff_impact_mean_terminal,
    Beta_eff_impact_pct_CI_1,
    Impact_convergence_shift,
    AR_Denominator,
    AR_Stable,
    Corr_gas_oil_regressors
  ) |>
  gt() |>
  fmt_number(
    columns = c(
      "Beta_eff_impact_mean_full",
      "Beta_eff_impact_mean_terminal",
      "Impact_convergence_shift",
      "AR_Denominator",
      "Corr_gas_oil_regressors"
    ),
    decimals = 3
  ) |>
  fmt_number(
    columns = "Beta_eff_impact_pct_CI_1",
    decimals = 1
  ) |>
  cols_label(
    Dependent = "Dep.",
    Independent = "Ind.",
    Beta_eff_impact_mean_full = md("Mean &beta;<sup>eff</sup>"),
    Beta_eff_impact_mean_terminal = md("Terminal &beta;<sup>eff</sup>"),
    Beta_eff_impact_pct_CI_1 = md("% CI &ni; 1"),
    Impact_convergence_shift = "Impact shift",
    AR_Denominator = md("1 − &phi;<sub>1</sub> − &phi;<sub>2</sub>"),
    AR_Stable = "AR stable",
    Corr_gas_oil_regressors = "corr(gas, oil)"
  ) |>
  cols_align(align = "left", columns = c("Dependent", "Independent", "AR_Stable")) |>
  tab_source_note(
    source_note =
      "Integration is assessed using the effective slope beta_eff = beta_base + sum_b omega_b S_b(t)."
  ) |>
  format_academic_table()

save_gt_safe(gt_int, file.path(tables_dir, "Integration_tests.png"))

# ------------------------------------------------------------------------------
# 6B. MLE parameters
# ------------------------------------------------------------------------------

mle_cols <- intersect(
  names(mle_pars),
  c(
    "Dependent", "Independent",
    "V_obs", "Q_beta", "Q_delta",
    "log_V_obs", "log_Q_beta", "log_Q_delta",
    "Q_beta_effectively_zero", "Q_delta_effectively_zero",
    "Phi1", "Phi1_SE", "Phi2", "Phi2_SE",
    "AR_Denominator", "AR_Min_Root_Modulus", "AR_Stable",
    "Corr_gas_oil_regressors",
    "N_regimes", "N_breaks", "N_states",
    "Neg_LogLik", "AIC_hyper", "BIC_hyper", "AIC_static", "BIC_static",
    "Converged"
  )
)

gt_mle <- mle_pars |>
  mutate(
    Dependent = hub_labels[Dependent],
    Independent = hub_labels[Independent]
  ) |>
  dplyr::select(all_of(mle_cols)) |>
  gt() |>
  fmt_scientific(
    columns = intersect(mle_cols, c("V_obs", "Q_beta", "Q_delta")),
    decimals = 2
  ) |>
  fmt_number(
    columns = intersect(
      mle_cols,
      c(
        "log_V_obs", "log_Q_beta", "log_Q_delta",
        "Phi1", "Phi1_SE", "Phi2", "Phi2_SE",
        "AR_Denominator", "AR_Min_Root_Modulus",
        "Corr_gas_oil_regressors"
      )
    ),
    decimals = 4
  ) |>
  fmt_number(
    columns = intersect(mle_cols, c("Neg_LogLik", "AIC_hyper", "BIC_hyper", "AIC_static", "BIC_static")),
    decimals = 1
  ) |>
  cols_label(
    Dependent = "Dep.",
    Independent = "Ind.",
    V_obs = md("H"),
    Q_beta = md("Q<sub>&beta;</sub>"),
    Q_delta = md("Q<sub>&delta;</sub>"),
    log_V_obs = md("log H"),
    log_Q_beta = md("log Q<sub>&beta;</sub>"),
    log_Q_delta = md("log Q<sub>&delta;</sub>"),
    Q_beta_effectively_zero = md("Q<sub>&beta;</sub> ≈ 0"),
    Q_delta_effectively_zero = md("Q<sub>&delta;</sub> ≈ 0"),
    Phi1 = md("&phi;<sub>1</sub>"),
    Phi1_SE = md("SE(&phi;<sub>1</sub>)"),
    Phi2 = md("&phi;<sub>2</sub>"),
    Phi2_SE = md("SE(&phi;<sub>2</sub>)"),
    AR_Denominator = md("1 − &phi;<sub>1</sub> − &phi;<sub>2</sub>"),
    AR_Min_Root_Modulus = "Min root",
    AR_Stable = "AR stable",
    Corr_gas_oil_regressors = "corr(gas, oil)",
    N_regimes = "Regimes",
    N_breaks = "Breaks",
    N_states = "States",
    Neg_LogLik = "−log L",
    AIC_hyper = "AIC hyper",
    BIC_hyper = "BIC hyper",
    AIC_static = "AIC static",
    BIC_static = "BIC static",
    Converged = "Conv."
  ) |>
  cols_align(
    align = "left",
    columns = intersect(
      mle_cols,
      c(
        "Dependent", "Independent", "Converged",
        "Q_beta_effectively_zero", "Q_delta_effectively_zero", "AR_Stable"
      )
    )
  ) |>
  tab_source_note(
    source_note =
      "H, Q_beta, and Q_delta are the estimated measurement and state-innovation variances. A near-zero Q_beta means the random-walk beta path contributes little beyond the spread-break slope dummies."
  ) |>
  format_academic_table()

save_gt_safe(gt_mle, file.path(tables_dir, "MLE_parameters.png"))

# ------------------------------------------------------------------------------
# 6D. Residual diagnostics
# ------------------------------------------------------------------------------

gt_diag <- diag_df |>
  mutate(
    Pair = paste0(hub_labels[Dependent], " ~ ", hub_labels[Independent]),
    LB_sig = add_stars(LB_p),
    LB_sq_sig = add_stars(LB_sq_p),
    JB_sig = add_stars(JB_p)
  ) |>
  dplyr::select(
    Pair,
    LB_stat, LB_p, LB_sig,
    LB_sq_stat, LB_sq_p, LB_sq_sig,
    JB_stat, JB_p, JB_sig
  ) |>
  gt() |>
  fmt_number(columns = c("LB_stat", "LB_sq_stat", "JB_stat"), decimals = 2) |>
  fmt_number(columns = c("LB_p", "LB_sq_p", "JB_p"), decimals = 3) |>
  cols_label(
    Pair = "Pair",
    LB_stat = "Q(20)",
    LB_p = "p",
    LB_sig = "",
    LB_sq_stat = "Q(20)",
    LB_sq_p = "p",
    LB_sq_sig = "",
    JB_stat = "JB",
    JB_p = "p",
    JB_sig = ""
  ) |>
  tab_spanner(
    label = "Ljung–Box residuals",
    columns = c("LB_stat", "LB_p", "LB_sig")
  ) |>
  tab_spanner(
    label = "Ljung–Box squared residuals",
    columns = c("LB_sq_stat", "LB_sq_p", "LB_sq_sig")
  ) |>
  tab_spanner(
    label = "Jarque–Bera",
    columns = c("JB_stat", "JB_p", "JB_sig")
  ) |>
  cols_align(align = "left", columns = c("Pair", "LB_sig", "LB_sq_sig", "JB_sig")) |>
  tab_source_note(
    source_note =
      "Ljung–Box tests the null of no autocorrelation up to 20 lags. The squared-residual version is a volatility-clustering diagnostic. Jarque–Bera tests normality."
  ) |>
  tab_source_note(
    source_note =
      "***, **, * denote rejection at the 1%, 5%, and 10% levels."
  ) |>
  format_academic_table()

save_gt_safe(gt_diag, file.path(tables_dir, "Residual_diagnostics.png"))

# ------------------------------------------------------------------------------
# 6E. PACF diagnostics
# ------------------------------------------------------------------------------

pcf_file <- file.path(kf_model_dir, "pacf_diagnostics.csv")

if (file.exists(pcf_file)) {
  pacf_diag <- read_csv(pcf_file, show_col_types = FALSE)

  pacf_lags_1_10 <- pacf_diag |>
    filter(Lag >= 1, Lag <= 10)

  pacf_wide <- pacf_lags_1_10 |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent],
      Pair = paste0(Dependent, " ~ ", Independent),
      Lag_col = paste0("Lag_", Lag)
    ) |>
    dplyr::select(Pair, Lag_col, PACF, Significant) |>
    pivot_wider(
      names_from = Lag_col,
      values_from = c(PACF, Significant),
      names_vary = "slowest"
    )

  residuals_stats <- residuals_df |>
    group_by(Dependent, Independent) |>
    filter(is.finite(Residual_Std)) |>
    summarise(
      N = n(),
      CI_bound = 1.96 / sqrt(n()),
      .groups = "drop"
    ) |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent],
      Pair = paste0(Dependent, " ~ ", Independent)
    ) |>
    dplyr::select(Pair, N, CI_bound)

  lb_stats <- residuals_df |>
    group_by(Dependent, Independent) |>
    filter(is.finite(Residual_Std)) |>
    summarise(
      {
        res_clean <- na.omit(Residual_Std)
        if (length(res_clean) > 5) {
          lb5 <- Box.test(res_clean, lag = 5, type = "Ljung-Box")
          lb10 <- Box.test(res_clean, lag = 10, type = "Ljung-Box")
          tibble(
            LB5_stat = lb5$statistic,
            LB5_p = lb5$p.value,
            LB10_stat = lb10$statistic,
            LB10_p = lb10$p.value
          )
        } else {
          tibble(
            LB5_stat = NA_real_,
            LB5_p = NA_real_,
            LB10_stat = NA_real_,
            LB10_p = NA_real_
          )
        }
      },
      .groups = "drop"
    ) |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent],
      Pair = paste0(Dependent, " ~ ", Independent),
      LB5_sig = add_stars(LB5_p),
      LB10_sig = add_stars(LB10_p)
    ) |>
    dplyr::select(Pair, LB5_stat, LB5_p, LB5_sig, LB10_stat, LB10_p, LB10_sig)

  pacf_table_data <- pacf_wide |>
    left_join(residuals_stats, by = "Pair") |>
    left_join(lb_stats, by = "Pair")

  lag_cols <- paste0("PACF_Lag_", 1:10)
  sig_cols <- paste0("Significant_Lag_", 1:10)

  gt_pacf <- pacf_table_data |>
    dplyr::select(
      Pair,
      all_of(lag_cols),
      CI_bound,
      LB5_stat, LB5_p, LB5_sig,
      LB10_stat, LB10_p, LB10_sig
    ) |>
    mutate(
      across(all_of(lag_cols), ~ifelse(is.na(.), NA, round(., 3)))
    ) |>
    gt() |>
    fmt_number(columns = all_of(lag_cols), decimals = 3) |>
    fmt_number(columns = c("CI_bound"), decimals = 3) |>
    fmt_number(
      columns = c("LB5_stat", "LB10_stat"),
      decimals = 2
    ) |>
    fmt_number(
      columns = c("LB5_p", "LB10_p"),
      decimals = 3
    ) |>
    cols_label(
      Pair = "Pair",
      PACF_Lag_1 = "Lag 1",
      PACF_Lag_2 = "Lag 2",
      PACF_Lag_3 = "Lag 3",
      PACF_Lag_4 = "Lag 4",
      PACF_Lag_5 = "Lag 5",
      PACF_Lag_6 = "Lag 6",
      PACF_Lag_7 = "Lag 7",
      PACF_Lag_8 = "Lag 8",
      PACF_Lag_9 = "Lag 9",
      PACF_Lag_10 = "Lag 10",
      CI_bound = "95% bound",
      LB5_stat = "Q(5)",
      LB5_p = "p",
      LB5_sig = "",
      LB10_stat = "Q(10)",
      LB10_p = "p",
      LB10_sig = ""
    ) |>
    tab_spanner(
      label = "Partial autocorrelation",
      columns = c(
        "PACF_Lag_1", "PACF_Lag_2", "PACF_Lag_3", "PACF_Lag_4",
        "PACF_Lag_5", "PACF_Lag_6", "PACF_Lag_7", "PACF_Lag_8",
        "PACF_Lag_9", "PACF_Lag_10", "CI_bound"
      )
    ) |>
    tab_spanner(
      label = "Ljung–Box",
      columns = c("LB5_stat", "LB5_p", "LB5_sig", "LB10_stat", "LB10_p", "LB10_sig")
    ) |>
    cols_align(
      align = "left",
      columns = c("Pair", "LB5_sig", "LB10_sig")
    ) |>
    tab_source_note(
      source_note =
        "Partial autocorrelation function (PACF) of standardised Kalman filter innovations at lags 1–10. Bold values exceed the 95% significance bound (±1.96/√T). Ljung–Box tests the null of no autocorrelation up to the indicated lag. ***, **, * denote rejection at the 1%, 5%, and 10% levels."
    ) |>
    format_academic_table()

  save_gt_safe(gt_pacf, file.path(tables_dir, "PACF_diagnostics.png"))
} else {
  message("pacf_diagnostics.csv not found. Skipping PACF diagnostics table.")
}

# ------------------------------------------------------------------------------
# 6F. ACF diagnostics
# ------------------------------------------------------------------------------

acf_file_tbl <- file.path(kf_model_dir, "acf_diagnostics.csv")

if (file.exists(acf_file_tbl)) {
  acf_diag_tbl <- read_csv(acf_file_tbl, show_col_types = FALSE)

  acf_lags_1_10 <- acf_diag_tbl |>
    filter(Lag >= 1, Lag <= 10)

  acf_wide <- acf_lags_1_10 |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent],
      Pair = paste0(Dependent, " ~ ", Independent),
      Lag_col = paste0("Lag_", Lag)
    ) |>
    dplyr::select(Pair, Lag_col, ACF, Significant) |>
    pivot_wider(
      names_from = Lag_col,
      values_from = c(ACF, Significant),
      names_vary = "slowest"
    )

  residuals_stats_acf <- residuals_df |>
    group_by(Dependent, Independent) |>
    filter(is.finite(Residual_Std)) |>
    summarise(
      N = n(),
      CI_bound = 1.96 / sqrt(n()),
      .groups = "drop"
    ) |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent],
      Pair = paste0(Dependent, " ~ ", Independent)
    ) |>
    dplyr::select(Pair, N, CI_bound)

  lb_stats_acf <- residuals_df |>
    group_by(Dependent, Independent) |>
    filter(is.finite(Residual_Std)) |>
    summarise(
      {
        res_clean <- na.omit(Residual_Std)
        if (length(res_clean) > 5) {
          lb5 <- Box.test(res_clean, lag = 5, type = "Ljung-Box")
          lb10 <- Box.test(res_clean, lag = 10, type = "Ljung-Box")
          tibble(
            LB5_stat = lb5$statistic,
            LB5_p = lb5$p.value,
            LB10_stat = lb10$statistic,
            LB10_p = lb10$p.value
          )
        } else {
          tibble(
            LB5_stat = NA_real_,
            LB5_p = NA_real_,
            LB10_stat = NA_real_,
            LB10_p = NA_real_
          )
        }
      },
      .groups = "drop"
    ) |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent],
      Pair = paste0(Dependent, " ~ ", Independent),
      LB5_sig = add_stars(LB5_p),
      LB10_sig = add_stars(LB10_p)
    ) |>
    dplyr::select(Pair, LB5_stat, LB5_p, LB5_sig, LB10_stat, LB10_p, LB10_sig)

  acf_table_data <- acf_wide |>
    left_join(residuals_stats_acf, by = "Pair") |>
    left_join(lb_stats_acf, by = "Pair")

  acf_lag_cols <- paste0("ACF_Lag_", 1:10)

  gt_acf <- acf_table_data |>
    dplyr::select(
      Pair,
      all_of(acf_lag_cols),
      CI_bound,
      LB5_stat, LB5_p, LB5_sig,
      LB10_stat, LB10_p, LB10_sig
    ) |>
    mutate(
      across(all_of(acf_lag_cols), ~ifelse(is.na(.), NA, round(., 3)))
    ) |>
    gt() |>
    fmt_number(columns = all_of(acf_lag_cols), decimals = 3) |>
    fmt_number(columns = c("CI_bound"), decimals = 3) |>
    fmt_number(
      columns = c("LB5_stat", "LB10_stat"),
      decimals = 2
    ) |>
    fmt_number(
      columns = c("LB5_p", "LB10_p"),
      decimals = 3
    ) |>
    cols_label(
      Pair = "Pair",
      ACF_Lag_1 = "Lag 1",
      ACF_Lag_2 = "Lag 2",
      ACF_Lag_3 = "Lag 3",
      ACF_Lag_4 = "Lag 4",
      ACF_Lag_5 = "Lag 5",
      ACF_Lag_6 = "Lag 6",
      ACF_Lag_7 = "Lag 7",
      ACF_Lag_8 = "Lag 8",
      ACF_Lag_9 = "Lag 9",
      ACF_Lag_10 = "Lag 10",
      CI_bound = "95% bound",
      LB5_stat = "Q(5)",
      LB5_p = "p",
      LB5_sig = "",
      LB10_stat = "Q(10)",
      LB10_p = "p",
      LB10_sig = ""
    ) |>
    tab_spanner(
      label = "Autocorrelation",
      columns = c(
        "ACF_Lag_1", "ACF_Lag_2", "ACF_Lag_3", "ACF_Lag_4",
        "ACF_Lag_5", "ACF_Lag_6", "ACF_Lag_7", "ACF_Lag_8",
        "ACF_Lag_9", "ACF_Lag_10", "CI_bound"
      )
    ) |>
    tab_spanner(
      label = "Ljung–Box",
      columns = c("LB5_stat", "LB5_p", "LB5_sig", "LB10_stat", "LB10_p", "LB10_sig")
    ) |>
    cols_align(
      align = "left",
      columns = c("Pair", "LB5_sig", "LB10_sig")
    ) |>
    tab_source_note(
      source_note =
        "Autocorrelation function (ACF) of standardised Kalman filter innovations at lags 1–10. Bold values exceed the 95% significance bound (±1.96/√T). Ljung–Box tests the null of no autocorrelation up to the indicated lag. ***, **, * denote rejection at the 1%, 5%, and 10% levels."
    ) |>
    format_academic_table()

  save_gt_safe(gt_acf, file.path(tables_dir, "ACF_diagnostics.png"))
} else {
  message("acf_diagnostics.csv not found. Skipping ACF diagnostics table.")
}

# ------------------------------------------------------------------------------
# 6G. Regime alphas
# ------------------------------------------------------------------------------

ra_file <- file.path(kf_model_dir, "regime_alphas.csv")

if (file.exists(ra_file)) {
  ra <- read_csv(ra_file, show_col_types = FALSE)

  gt_ra <- ra |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent]
    ) |>
    gt() |>
    fmt_number(columns = c("Alpha", "SE"), decimals = 4) |>
    cols_label(
      Dependent = "Dep.",
      Independent = "Ind.",
      Regime = "Regime",
      Alpha = md("&alpha;"),
      SE = md("SE(&alpha;)")
    ) |>
    cols_align(align = "left", columns = c("Dependent", "Independent", "Regime")) |>
    tab_source_note(
      source_note =
        "Regime-specific intercepts in the measurement equation. Regimes are delimited by Bai–Perron break dates on the log-price spread for each pair."
    ) |>
    format_academic_table()

  save_gt_safe(gt_ra, file.path(tables_dir, "Regime_alphas.png"))
}

# ------------------------------------------------------------------------------
# 6H. Dummy coefficients
# ------------------------------------------------------------------------------

dc_file <- file.path(kf_model_dir, "dummy_coefficients.csv")

if (file.exists(dc_file)) {
  dc <- read_csv(dc_file, show_col_types = FALSE) |>
    mutate(
      Dependent = hub_labels[Dependent],
      Independent = hub_labels[Independent],
      Break_date = as.Date(Break_date),
      Z = Omega / SE,
      p_value = 2 * pnorm(-abs(Z)),
      Sig = add_stars(p_value)
    )

  dc_cols <- c(
    "Dependent", "Independent", "Break_num", "Break_date",
    "Omega", "SE", "Z", "p_value", "Sig"
  )

  dc_cols <- intersect(dc_cols, names(dc))

  gt_dc <- dc |>
    dplyr::select(all_of(dc_cols)) |>
    gt() |>
    fmt_number(columns = intersect(dc_cols, c("Omega", "SE", "Z")), decimals = 4) |>
    fmt_number(columns = intersect(dc_cols, "p_value"), decimals = 3) |>
    fmt_date(columns = intersect(dc_cols, "Break_date"), date_style = "yMMMd") |>
    cols_label(
      Dependent = "Dep.",
      Independent = "Ind.",
      Break_num = "#",
      Break_date = "Break date",
      Omega = md("&omega;"),
      SE = md("SE(&omega;)"),
      Z = md("z"),
      p_value = md("p"),
      Sig = ""
    ) |>
    cols_align(
      align = "left",
      columns = intersect(dc_cols, c("Dependent", "Independent", "Break_date", "Sig"))
    ) |>
    tab_source_note(
      source_note =
        "Omega coefficients are permanent slope shifts entering beta_eff = beta_base + sum_b omega_b S_b(t). Break dates come from Bai–Perron analysis of the log-price spread for each pair. The z-statistics use smoothed-state standard errors at the end of the sample."
    ) |>
    tab_source_note(
      source_note =
        "***, **, * denote rejection of H0: omega = 0 at the 1%, 5%, and 10% levels."
    ) |>
    format_academic_table()

  save_gt_safe(gt_dc, file.path(tables_dir, "Dummy_coefficients.png"))
}

# Done

cat("\n== Dummy Kalman Filter visualisation finished ==\n")
cat("   Plots written to:  ", plots_dir, "\n")
cat("   Tables written to: ", tables_dir, "\n")
cat("   Residuals written to: ", resid_dir, "\n")