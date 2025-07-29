# Maternal Health Coverage Analysis

This repository contains an R-based analysis of global maternal healthcare coverage, focusing on:

- *ANC4+ Coverage*: Percentage of women receiving ≥4 antenatal visits
- *Skilled Birth Attendance (SBA)*: Percentage of deliveries attended by skilled personnel

The goal is to evaluate differences in service coverage between *on-track* and *off-track* countries based on the Under-5 Mortality Rate (U5MR) status.

## 📁 Repository Structure

- 01_rawdata/ – Source Excel data from UNICEF and UN WPP
- 03_scripts/ – Cleaning and analysis code
- outputs/ – Final PDF/HTML reports and plots
- test.Rmd – R Markdown script for full report generation

## 📊 Key Insights

- Population-weighted estimates reveal *higher ANC4+ and SBA coverage* in on-track countries.
- Off-track countries lag significantly, particularly in ANC4+.
- Recovery trends (2019–2022) show regional disparities and resilience gaps post-pandemic.

## 📦 Requirements

This project uses R (≥4.0.0) with the following key packages:
- tidyverse
- readxl
- sf
- ggplot2
- rnaturalearth

Install packages using:

```r
install.packages(c(\"tidyverse\", \"readxl\", \"sf\", \"rnaturalearth\", \"ggrepel\"))
