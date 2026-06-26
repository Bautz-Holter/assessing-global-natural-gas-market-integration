#!/usr/bin/env python3
"""
run.py — Pipeline runner for the Natural Gas Spot Market model.

Each command runs the corresponding R script(s) via Rscript with the
repo root as the working directory (required by here::here()).

Usage examples
--------------
  python run.py setup
  python run.py clean
  python run.py describe
  python run.py unitroot
  python run.py kalman
  python run.py kalman --family m1   # standard (no oil)
  python run.py kalman --family m2   # standard with oil
  python run.py kalman --family m3   # dummy variable
  python run.py pipeline

Global flags
------------
  --rscript PATH  Override Rscript binary location
"""

import argparse
import glob
import subprocess
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Repo root — the directory that contains this script
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parent

# ---------------------------------------------------------------------------
# Script map
# ---------------------------------------------------------------------------
STAGE_SCRIPTS = {
    "clean": {
        "model": "Model/Code/Data Cleaning.R",
        "viz":   "Visualization/Code/Data Cleaning.r",
    },
    "describe": {
        "model": "Model/Code/Descriptive Analysis.r",
        "viz":   "Visualization/Code/Descriptive Analysis.r",
    },
    "unitroot": {
        "model": "Model/Code/Unit Root and Structural Break.r",
        "viz":   "Visualization/Code/Unit Root and Structural Break.r",
    },
}

# Kalman scripts keyed by family (m1 / m2 / m3)
KALMAN_SCRIPTS = {
    "m1": {
        "model": "Model/Code/Kalman Filter/Standard Kalman Filter.r",
        "viz":   "Visualization/Code/Kalman filter/Standard Kalman.r",
    },
    "m2": {
        "model": "Model/Code/Kalman Filter/Standard Kalman Filter with oil.r",
        "viz":   "Visualization/Code/Kalman filter/Standard Kalman Filter with oil.r",
    },
    "m3": {
        "model": "Model/Code/Kalman Filter/Dummy Kalman Filter.r",
        "viz":   "Visualization/Code/Kalman filter/Dummy Kalman Filter.r",
    },
}

FAMILIES = ["m1", "m2", "m3"]

# Default pipeline order
DEFAULT_PIPELINE = ["clean", "describe", "unitroot", "kalman"]

# ---------------------------------------------------------------------------
# Rscript detection
# ---------------------------------------------------------------------------

def find_rscript(override):
    """Return path to Rscript, or exit with a clear error."""
    if override:
        return override

    import shutil
    found = shutil.which("Rscript")
    if found:
        return found

    # Windows default install locations (glob latest version)
    patterns = [
        r"C:/Program Files/R/R-*/bin/Rscript.exe",
        r"C:/Program Files/R/R-*/bin/x64/Rscript.exe",
    ]
    candidates = []
    for pat in patterns:
        candidates.extend(glob.glob(pat))
    if candidates:
        candidates.sort(reverse=True)
        return candidates[0]

    print(
        "ERROR: Rscript not found on PATH or in default Windows install locations.\n"
        "       Install R from https://cran.r-project.org/ or pass --rscript <path>.",
        file=sys.stderr,
    )
    sys.exit(1)


# ---------------------------------------------------------------------------
# Execution helpers
# ---------------------------------------------------------------------------

def run_script(rscript, rel_path, label):
    """Run a single R script. Exits immediately on non-zero return code."""
    script_path = REPO_ROOT / rel_path

    if not script_path.exists():
        print(f"  ERROR: script not found: {rel_path}", file=sys.stderr)
        sys.exit(1)

    cmd = [rscript, "--no-save", "--no-restore", str(script_path)]
    print(f"  [{label}] Rscript {rel_path}")

    result = subprocess.run(cmd, cwd=str(REPO_ROOT))
    if result.returncode != 0:
        print(
            f"\n  ERROR: script exited with code {result.returncode}: {rel_path}",
            file=sys.stderr,
        )
        sys.exit(result.returncode)


def run_stage(stage, rscript, family="all"):
    """Dispatch a single named stage."""
    print(f"\n=== {stage.upper()} ===")

    if stage == "kalman":
        _run_kalman(rscript, family)
        return

    entry = STAGE_SCRIPTS[stage]
    run_script(rscript, entry["model"], "model")
    run_script(rscript, entry["viz"],   "viz")


def _run_kalman(rscript, family):
    families = FAMILIES if family == "all" else [family]
    for fam in families:
        entry = KALMAN_SCRIPTS[fam]
        run_script(rscript, entry["model"], f"model  {fam}")
        run_script(rscript, entry["viz"],   f"viz    {fam}")


# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

def add_global_flags(parser):
    parser.add_argument("--rscript", metavar="PATH", help="Path to Rscript executable")


def build_parser():
    parser = argparse.ArgumentParser(
        prog="run.py",
        description="Pipeline runner for the Natural Gas Spot Market model.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    sub = parser.add_subparsers(dest="command", metavar="command", required=True)

    # --- simple stages ---
    for name, help_text in [
        ("clean",    "Run Data Cleaning"),
        ("describe", "Run Descriptive Analysis"),
        ("unitroot", "Run Unit Root and Structural Break"),
    ]:
        p = sub.add_parser(name, help=help_text)
        add_global_flags(p)

    # --- setup ---
    p_setup = sub.add_parser(
        "setup",
        help="Install all R package dependencies into a project-local renv library",
    )
    add_global_flags(p_setup)

    # --- kalman ---
    p_kalman = sub.add_parser("kalman", help="Run Kalman Filter models")
    add_global_flags(p_kalman)
    p_kalman.add_argument(
        "--family",
        choices=FAMILIES + ["all"],
        default="all",
        help="Model family: m1 | m2 | m3 (default: all)",
    )

    # --- pipeline ---
    p_pipe = sub.add_parser(
        "pipeline",
        help=f"Run all stages in order: {' -> '.join(DEFAULT_PIPELINE)}",
    )
    add_global_flags(p_pipe)
    p_pipe.add_argument(
        "--family",
        choices=FAMILIES + ["all"],
        default="all",
        help="Model family for the kalman stage: m1 | m2 | m3 (default: all)",
    )

    return parser


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    parser = build_parser()
    args = parser.parse_args()

    rscript = find_rscript(args.rscript)

    if args.command == "setup":
        print("\n=== SETUP ===")
        run_script(rscript, "dependencies.R", "renv")
        print("\nDone.")
        return

    family = getattr(args, "family", "all")

    if args.command == "pipeline":
        for stage in DEFAULT_PIPELINE:
            run_stage(stage, rscript, family=family)
    elif args.command == "kalman":
        run_stage("kalman", rscript, family=family)
    else:
        run_stage(args.command, rscript)

    print("\nDone.")


if __name__ == "__main__":
    main()
