# Sprint 4: Data Explorer & Final Assembly

## 1. Objective
Implement the data export tab and perform final package-wide checks.

## 2. Tasks
- **Tab 7 (`R/mod_data_explorer.R`):**
  - Implement a `DT::renderDataTable`.
  - **Feature:** Include a toggle to show "CTF Scores" vs. "Raw Values."
  - Ensure the table is filtered by the same sidebar logic (Country/Group/Year) used in the plots.
- **Final Polish:**
  - Apply `bslib::bs_theme(version = 5, bootswatch = "lux")` to the entire app.
  - Ensure all `plotly` tooltips are clean and informative.
- **Validation:**
  - Run `devtools::document()`, `devtools::install()`, and `devtools::check()`.
  - Confirm 100% test pass and 0 errors/warnings.


  Is anything unclear with the prompt? What do you think about the design of thus far app? Does it need improvements? What would you do differently? 
