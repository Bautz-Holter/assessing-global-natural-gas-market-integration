# Natural Gas Spot Market Integration

Master thesis repository — Magnus Bautz-Holter, Sondre Jentoft, Tallak Ravn  
Faculty of Economics and Management  
Department of Industrial Economics and Technology Management (IØT)  
Norwegian University of Science and Technology (NTNU)  
Master's Thesis in Industrial Economics and Technology Management with specialization in Finance, 2026

---

## Overview

This repository contains the full econometric analysis pipeline for studying natural gas spot market integration across four major trading hubs: JKM (Asia-Pacific), NBP (UK), TTF (Netherlands), and Henry Hub (US).

The study uses time-varying parameter state-space models (Kalman filter) with Bai-Perron structural breaks to measure the extent and dynamics of cross-market price integration, and to assess the role of oil-indexed long-term contracts in cross-market price transmission.

---

## Repository structure

```
Natural-gas-spot-market/
├── Model/
│   ├── Code/
│   │   ├── Data Cleaning.R
│   │   ├── Descriptive Analysis.r
│   │   ├── Unit Root and Structural Break.r
│   │   └── Kalman Filter/
│   │       ├── Standard Kalman Filter.r
│   │       ├── Standard Kalman Filter with oil.r
│   │       └── Dummy Kalman Filter.r
│   └── model output/                   # Generated CSVs
├── Visualization/
│   ├── Code/                           # Publication-ready plots and tables
│   └── Plot outputs/                   # Generated figures
├── Gas prices.xlsx                     # Raw hub price data
├── brent_crude_spot.xls                # EIA Brent crude spot prices (daily)
├── run.py                              # CLI pipeline runner (Python 3)
├── dependencies.R                      # R package manifest (renv)
├── renv.lock                           # Locked package versions
├── .Rprofile                           # Activates renv on R startup
├── renv/                               # renv infrastructure (library gitignored)
└── .gitignore
```

---

## Pipeline

Run stages in this order (each depends on the output of the previous):

| Step | Stage | Description |
|------|-------|-------------|
| 1 | `clean` | Merge hub prices and Brent crude; compute 63-business-day trailing MA of Brent; output `gas_clean_balanced.csv` and `gas_clean_ma3_balanced.csv` |
| 2 | `describe` | Summary statistics, correlations, spreads, Ljung-Box and ARCH tests |
| 3 | `unitroot` | ADF, PP, KPSS, Zivot-Andrews, and Bai-Perron structural break dating |
| 4 | `kalman` | Time-varying parameter Kalman filter — three variants across main and non-core families |

Each stage produces CSVs in `Model/model output/` and publication-ready figures in `Visualization/Plot outputs/`.

---

## Model variants

| Variant | Description |
|---------|-------------|
| `standard` (`m1`) | Time-varying beta, regime-specific intercepts, AR(2) component, no oil regressor |
| `oil` (`m2`) | Adds a time-varying Brent_MA3 coefficient (delta) to test oil-linked pricing |
| `dummy` (`m3`) | Adds permanent slope-shift dummies at Bai-Perron break dates alongside time-varying beta |

---

## Running the pipeline

Use `run.py` (Python 3 standard library only — no additional packages required):

```bash
# First-time setup: install all R packages into the project-local renv library
python run.py setup

# Full default pipeline: clean → describe → unitroot → kalman
python run.py pipeline

# Individual stages
python run.py clean
python run.py describe
python run.py unitroot
python run.py kalman

# Target a specific Kalman filter variant
python run.py kalman --family m1            # standard (no oil)
python run.py kalman --family m2            # standard with oil
python run.py kalman --family m3            # dummy variable
```

Run `python run.py --help` or `python run.py <command> --help` for the full option reference.

> **Note:** `Rscript` must be on your `PATH`, or installed in the default Windows location (`C:/Program Files/R/R-*/bin/`). Pass `--rscript <path>` to override.

---

## Requirements

### R packages

Package dependencies are managed via [renv](https://rstudio.github.io/renv/). Run the following once after cloning to install all packages into a project-local library:

```bash
python run.py setup
```

This reads `renv.lock` and restores the exact package versions used in the project. The full package list is declared in `dependencies.R`.

### Python

Python 3 (standard library only). Used exclusively for `run.py`.

---

## Data sources

| File | Series | Unit | Purpose | Source |
|------|--------|------|---------|--------|
| `Gas prices.xlsx` | JKM, NBP, TTF, Henry Hub daily spot prices | USD/mmBtu | Hub prices | London Stock Exchange Group |
| `brent_crude_spot.xls` | EIA Europe Brent Spot Price FOB (RBRTE) | USD/bbl → converted to USD/mmBtu (÷ 5.8) | Source for `Brent_crude` and `Brent_MA3` (63-day MA of spot prices) | U.S. Energy Information Administration |

`Brent_crude` (daily spot, converted to USD/mmBtu) and `Brent_MA3` (63-day trailing moving average of `Brent_crude`) are both derived from `brent_crude_spot.xls` in Data Cleaning. They serve different modelling purposes: `Brent_crude` is used in descriptive and unit root analysis; `Brent_MA3` is the oil-integration control in the Kalman filter oil and dummy variants.

---

## Documentation

For detailed information on model and visualization scripts, see:

- **[Model/Code/README.md](Model/Code/README.md)** — Data preparation, run order, key parameters, Kalman filter variants, troubleshooting
- **[Visualization/README.md](Visualization/README.md)** — How to run visualization scripts, design conventions, plot/table structure, LaTeX integration

---

## License

No license - use as you wish!

---

## Contact

Magnus Bautz-Holter  
Sondre Jentoft  
Tallak Ravn  
Norwegian University of Science and Technology (NTNU)
