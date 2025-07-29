# -------------------------------------------
# This script sets up the user environment
# for reproducible execution of the UNICEF D&A assessment
# -------------------------------------------

#  Required Packages
required_packages <- c(
  "tidyverse", "readxl", "ggplot2", "sf", "rnaturalearth", 
  "rnaturalearthdata", "ggrepel", "knitr", "rmarkdown"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}
invisible(lapply(required_packages, install_if_missing))

# Load libraries
invisible(lapply(required_packages, library, character.only = TRUE))

message("user_profile.R executed successfully.")
