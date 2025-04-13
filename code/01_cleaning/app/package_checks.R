# ---------------------------------------------------------------------------
# Script: package_checks.R
# Purpose: Manages package installation and loading for the project.
#          - Defines required packages (CRAN & GitHub).
#          - Ensures 'pak' package manager is installed.
#          - Installs/updates all required packages using 'pak'.
#          - Loads required libraries into the session.
# Usage:   Source this script using source("package_checks.R") at the
#          beginning of your main analysis script.
# ---------------------------------------------------------------------------

# --- Configuration: Define Required Packages ---
message("--- Defining Required Packages ---")
required_packages <- c(
  "tidyverse",              # For general data manipulation (incl. dplyr, ggplot2, etc.) - From CRAN
  "clessnverse/cartessn",   # Custom package - From GitHub
  "clessnverse/clessnize"   # Custom package - From GitHub
)

# --- Step 1: Ensure 'pak' Package Manager is Installed ---
message("\n--- Checking for 'pak' package manager ---")
if (!requireNamespace("pak", quietly = TRUE)) {
  message("Installing 'pak' package manager...")
  tryCatch({
    install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
    if (!requireNamespace("pak", quietly = TRUE)) {
      stop("Failed to install the 'pak' package automatically. \nPlease try installing it manually in the R console using:\ninstall.packages(\"pak\", repos = \"https://r-lib.github.io/p/pak/dev/\") \nand then re-run the script.")
    } else {
     message("'pak' package installed successfully.")
    }
  }, error = function(e) {
     stop("Error during 'pak' installation: ", e$message, "\nPlease try installing it manually.")
  })
} else {
   message("'pak' package is already installed.")
}

# --- Step 2: Install/Update Required Packages using 'pak' ---
message("\n--- Checking and Installing/Updating Required Packages using pak ---")
# Ensure pak is loaded to use its functions
# Suppress package startup messages for pak itself during this check phase
suppressPackageStartupMessages(library(pak))
tryCatch({
  pak::pkg_install(required_packages)
  message("Package check/installation complete.")
}, error = function(e) {
  stop("Failed to install/update required packages using pak. Error: ", e$message)
})

# --- Step 3: Load Required Libraries ---
# Load libraries needed for the main script AFTER ensuring they are installed.
# Suppress startup messages for cleaner output when sourced.
message("\n--- Loading Required Libraries ---")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(cartessn))
suppressPackageStartupMessages(library(clessnize))
message("Required libraries loaded successfully.")

# --- Package Check Script End ---
message("\n--- Package checks and loading finished ---")

# Clean up the package list variable from the environment if desired, though not strictly necessary
# rm(required_packages)
