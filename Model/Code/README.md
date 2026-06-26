# Model Code

Econometric analysis pipeline for natural gas spot market integration. All scripts read raw data or intermediate CSVs and write outputs to `Model/model output/`, organized by stage.

---

## Overview

The model pipeline progresses through four sequential stages:

1. **Data Cleaning** — Ingest xlsx/xls files, validate, convert units, compute rolling averages
2. **Descriptive Analysis** — Summary statistics, correlations, spreads, extreme returns
3. **Unit Root & Structural Break** — Stationarity tests, identify regime-shift dates (Bai-Perron)
4. **Kalman Filter (3 variants)** — State-space models with time-varying transmission parameters

**Core output:** Three Kalman filter variants, each producing state paths, MLE parameters, and integration tables across all 12 directed hub pairs.

---

## Running the Pipeline

Run stages sequentially from the repo root using the Python CLI:

```bash
python run.py pipeline          # full pipeline: clean → describe → unitroot → kalman
python run.py clean
python run.py describe
python run.py unitroot
python run.py kalman            # all Kalman variants (m1, m2, m3)
python run.py kalman --family m1   # standard (no oil)
python run.py kalman --family m2   # standard with oil
python run.py kalman --family m3   # dummy
```

Or call Rscript directly:

```bash
Rscript "Model/Code/Data Cleaning.R"
Rscript "Model/Code/Descriptive Analysis.r"
Rscript "Model/Code/Unit Root and Structural Break.r"
Rscript "Model/Code/Kalman Filter/Standard Kalman Filter.r"
Rscript "Model/Code/Kalman Filter/Standard Kalman Filter with oil.r"
Rscript "Model/Code/Kalman Filter/Dummy Kalman Filter.r"
```

Required packages: `readxl`, `dplyr`, `readr`, `tidyr`, `tibble`, `here`, `zoo`, `moments`, `tseries`, `FinTS`, `urca`, `strucchange`, `dlm`

---

## Scripts at a Glance

| Script | Purpose | Key Output |
|--------|---------|-----------|
| **Data Cleaning** | Load, validate, align datasets; compute Brent_MA3 | `gas_clean_balanced.csv`, `gas_clean_ma3_balanced.csv` |
| **Descriptive Analysis** | Establish baseline distributional and correlation facts | 19 CSV files with stats, correlations, spreads |
| **Unit Root & Break** | Test stationarity; find and date structural breaks | Break dates CSV, unit root tables |
| **Standard Kalman Filter** | Time-varying beta, regime intercepts, AR(2) | State paths for 12 directed pairs |
| **Standard Kalman with Oil** | Adds time-varying Brent_MA3 coefficient (delta) | State paths with delta states |
| **Dummy Kalman Filter** | Adds slope-shift dummies at Bai-Perron break dates | State paths with omega coefficients |

---

## Core Concepts

### Data Preparation
- **Input:** Gas prices (JKM, TTF, NBP, HH) from `Gas prices.xlsx`; Brent crude spot from `brent_crude_spot.xls`
- **Output:** Balanced daily panel (`gas_clean_balanced.csv`) and MA3-extended panel (`gas_clean_ma3_balanced.csv`)
- **Energy basis:** Brent crude converted from USD/bbl to USD/mmBtu using thermal factor 5.8 mmBtu/bbl

### Break Dates
- **Method:** Bai-Perron structural break test on pairwise log-price spreads (intercept-only specification)
- **Result:** Regime dummy indicators (R_k) and slope-shift step dummies (S_b) for use in the Kalman filter
- **Key output:** `spread_BaiPerron_breaks.csv` — consumed by all three Kalman filter variants

### Kalman Filter Variants

**Standard Kalman (m1):**
- Measurement: `log(P_dep) = sum_k[alpha_k * D_k(t)] + beta_t * log(P_ind) + phi1*y_{t-1} + phi2*y_{t-2} + eps`
- State: beta_t is a random walk; alpha_k and AR coefficients are fixed

**Standard with Oil (m2):**
- Adds a second time-varying state delta_t for the Brent_MA3 regressor
- Tests whether gas prices move with oil independently of other hub prices

**Dummy Kalman (m3):**
- Adds permanent slope-shift dummies at Bai-Perron break dates
- Compares sharp regime-shift integration against the gradual time-varying beta

---

## Data Dependencies

```
Raw Data (repo root):
├── Gas prices.xlsx          → JKM, TTF, NBP, HH daily spot prices
└── brent_crude_spot.xls     → Brent crude spot (Brent_crude and Brent_MA3)

Model/model output/
├── Data Cleaning/
│   ├── gas_clean_balanced.csv         (full sample; all columns incl. Brent_crude and Brent_MA3,
│   │                                   but Brent_MA3 is NA in the first ~63 rows due to MA burn-in)
│   ├── gas_clean_ma3_balanced.csv     (shorter sample; Brent_MA3 complete — no NAs)
│   └── gas_clean.csv                  (alias of gas_clean_balanced.csv)
├── Descriptive Analysis/
│   └── [19 CSV files]
└── Unit Root and Structural Break/
    └── Pairwise Spreads/
        └── spread_BaiPerron_breaks.csv   (break dates per pair — critical for Kalman)
```

**Which Kalman variant reads which file:**

| Variant | Input file | Rationale |
|---------|-----------|-----------|
| Standard KF (m1) | `gas_clean_ma3_balanced.csv` | Shorter but complete sample for consistent estimation window |
| Standard KF with oil (m2) | `gas_clean_ma3_balanced.csv` | Requires complete Brent_MA3 column |
| Dummy KF (m3) | `gas_clean_balanced.csv` | Uses full sample; Brent_MA3 with early NAs handled internally |

**Critical:** Always run `Data Cleaning.R` first. All downstream stages depend on its outputs.

---

## Key Parameters

### Brent_MA3 Rolling Window
- **Window size:** 63 business days (approximately one quarter)
- **Rationale:** Smooths short-term oil price noise; captures medium-run oil-contract pricing
- **Computation:** `zoo::rollmean(k = 63)` in `Data Cleaning.R`; Q4 2015 used as burn-in

### Thermal Conversion
- **Factor:** 5.8 mmBtu/bbl (EIA standard)
- **Applied to:** Brent crude only; gas hub prices are already in USD/mmBtu

### MLE Estimation
- Two-stage coarse-to-fine grid search over hyperparameters (log V_obs, log Q_beta, log Q_delta)
- Coarse stage: Nelder-Mead, max 50 iterations; fine stage: top-5 seeds, max 500 iterations
- Convergence flag reported in `mle_parameters.csv`

---

## Output Structure

```
Model/model output/
├── Data Cleaning/
│   ├── gas_clean_balanced.csv
│   ├── gas_clean_ma3_balanced.csv
│   ├── gas_clean_raw_aligned.csv
│   ├── gas_clean.csv                       (alias of gas_clean_balanced.csv)
│   ├── cleaning_log.txt
│   └── [missingness diagnostics — 8 CSVs]
├── Descriptive Analysis/
│   └── [correlation, spread, volatility, return CSVs]
├── Unit Root and Structural Break/
│   ├── unit_root_break_summary.txt
│   ├── Unit_Root/
│   ├── Bai_Perron/
│   ├── Pairwise_Spreads/
│   │   └── spread_BaiPerron_breaks.csv
│   ├── Break_Alignment/
│   └── Robustness/
└── Kalman filter/
    ├── Standard Kalman Filter/
    │   ├── state_paths.csv
    │   ├── mle_parameters.csv
    │   ├── integration_tests.csv
    │   ├── standardised_residuals.csv
    │   ├── innovations.csv
    │   ├── regime_alphas.csv
    │   ├── model_diagnostics.csv
    │   └── residuals_by_window.csv
    ├── Standard Kalman Filter with oil/
    │   └── [same structure + delta states in state_paths]
    └── Dummy Kalman Filter/
        └── [same structure + omega coefficients + dummy_coefficients.csv]
```

All outputs are committed to the repository and regenerated by running source scripts.

---

## Run Order

```
Data Cleaning.R
    ↓
Descriptive Analysis.r
Unit Root and Structural Break.r
    ↓
Kalman Filter/ (any or all of the three variants)
```

**Do not skip Data Cleaning.** All downstream stages depend on its outputs.

---

## Troubleshooting

**Data Cleaning fails?**
- Verify input files are in repo root: `Gas prices.xlsx`, `brent_crude_spot.xls`
- Check that date formats and column names in the source files match expectations

**Kalman filter fails with "missing Brent_MA3"?**
- Re-run `Data Cleaning.R` to ensure `gas_clean_ma3_balanced.csv` is generated with the MA3 column

**Kalman MLE doesn't converge?**
- Expected for some pairs; the coarse-to-fine grid search includes a fallback
- Convergence flag is reported in `mle_parameters.csv`; non-convergent results should be treated with caution

**Break dates missing or empty?**
- Check `spread_BaiPerron_breaks.csv` exists and is non-empty after running `Unit Root and Structural Break.r`
- The Dummy Kalman Filter filters out breaks at sample boundaries to avoid empty regimes

---

## Model Equations

### Standard Kalman Filter

$$\log(P_{d,t}) = \sum_k \alpha_k D_k(t) + \beta_t \log(P_{i,t}) + \phi_1 y_{t-1} + \phi_2 y_{t-2} + \varepsilon_t$$

where $\beta_t = \beta_{t-1} + \eta_t$ (random walk) and $\alpha_k$ are regime-specific intercepts.

### With Oil

$$\log(P_{d,t}) = \sum_k \alpha_k D_k(t) + \beta_t \log(P_{i,t}) + \delta_t \log(\text{Brent\_MA3}_t) + \phi_1 y_{t-1} + \phi_2 y_{t-2} + \varepsilon_t$$

where both $\beta_t$ and $\delta_t$ follow independent random walks.

### Dummy Kalman

$$\log(P_{d,t}) = \sum_k \alpha_k R_k(t) + \left(\beta_t^{\text{base}} + \sum_b \omega_b S_b(t)\right) \log(P_{i,t}) + \delta_t \log(\text{Brent\_MA3}_t) + \phi_1 y_{t-1} + \phi_2 y_{t-2} + \varepsilon_t$$

where $R_k(t)$ are regime dummies, $S_b(t)$ are permanent slope-shift step dummies, and $\omega_b$ captures the magnitude of integration change at each break.

---

## Integration Interpretation

**Beta ≈ 1:** Long-run price proportionality — consistent with integrated markets  
**Beta < 1:** Incomplete transmission — segmented markets or persistent price frictions  
**Long-run beta = beta / (1 − phi1 − phi2):** Total effect after AR(2) dynamics  
**Delta > 0:** Oil-linked pricing — gas prices co-move with oil beyond hub-to-hub transmission  
**Omega:** Slope shift at break date — positive omega indicates increasing integration post-break
