# Sprint 1: Foundation & Visualization Engine

## 1. Objective
Initialize the `cgjrapp` R package and build the primary plotting utility that handles the dual "Evolution" and "Line" views. The home page is already set up. We need to prepare the rest of the tabs. 

You need to study the lazyloaded data within cgjrdata package and make sure you understand its structure thoroughly. It will form the basis for all the analytics and plotting going forward. I can help you in the console with whatever code you need to run to teach yourself the data. 

## 2. Tasks
- **Infrastructure:** Set up `DESCRIPTION`, `NAMESPACE`, and `R/globals.R` to handle tidyverse non-standard evaluation (NSE) warnings, if they have not already been properly done. 
- **Data Helpers (`R/utils_data.R`):**
  - Create `filter_cgjr_data(data, primary_iso, benchmark_groups, custom_isos, year_range)`.
  - **Logic:** Filter by the user-selected years and countries. Assume regional/income averages are precomputed rows in the `cgjrdata` package.
- **Plotting Engine (`R/utils_plotting.R`):**
  - Create `plot_cgjr_master(data, primary_iso, view_mode, y_var)`.
  - **Common Elements:** `geom_rect` for Capacity Zones: Low (0-0.33), Emerging (0.34-0.66), High (0.67-1.0).
  - **Mode "evolution":** - Render vertical dot plots for all countries in selected benchmark groups (low alpha).
    - Render a bold connected path + larger points for `primary_iso` ONLY.
  - **Mode "line":**
    - Render solid lines for `primary_iso` and `custom_isos`.
    - Render distinct dashed lines for precomputed regional/income averages.
- **Documentation & Testing:**
  - Full Roxygen2 headers.
  - `testthat` coverage ensuring the year slider bounds correctly filter the plot data.

Given everything you know about the data, does this make sense? You are an R Shiny Expert with experience developing production grade visualizations for World Bank governance specialists. Is anything unclear with the prompt? What do you think about the design of thus far app? Does it need improvements? What would you do differently? 
