# Sprint 6: Data Explorer & Final Assembly

## Objective
Add a Data Explorer tab giving users transparent access to the underlying data, then perform full package validation. This sprint is complete when the app passes `devtools::check()` with 0 errors/0 warnings, all tests pass, and the app is ready for publication.

## Context
- This is the final tab — a transparency/download layer for power users
- The same shared sidebar state (country, peers, year range) applies here too
- Users should be able to toggle between CTF scores and raw values
- `rawdata_list` and `ctfdata_list` are the two data sources for this tab

## Tasks

### `R/mod_data_explorer.R`

#### `mod_data_explorer_ui(id)`
- `bslib::layout_sidebar()`:
  - **Sidebar:** 
    - `radioButtons` to toggle `"CTF Scores"` vs `"Raw Values"`
    - `selectInput` to choose cluster (drives subcluster choice below)
    - `selectInput` to choose subcluster (dynamically updated based on cluster)
    - Download button: `shiny::downloadButton()`
  - **Main:** `DT::DTOutput()` for the data table

#### `mod_data_explorer_server(id, primary_iso, benchmark_isos, year_range)`
- Source data:
  - `"CTF Scores"`: pull from `ctfdata_list[[cluster]][[subcluster]]`
  - `"Raw Values"`: pull from `rawdata_list[[cluster]][[subcluster]]`
- Filter using `filter_cgjr_data()` (same shared state)
- Render with `DT::renderDT()`:
  - Pagination, search, column sorting enabled
  - Numeric columns rounded to 3 decimal places
  - `country_type` column used for row highlighting (primary country highlighted)
- `downloadHandler()`: exports filtered table as `.csv`
- Subcluster `selectInput` updates dynamically when cluster changes (use `shiny::updateSelectInput()`)

### Final Assembly & Polish
- Review all `plotly` tooltip text — ensure they show country name, year, and score value
- Confirm `bslib::bs_theme(bootswatch = "litera")` is consistent across all tabs
- Confirm `thematic::thematic_shiny()` applies correctly to all ggplot-based plots
- Update `DESCRIPTION` to final version (version bump to `0.1.0`)
- Run `devtools::document()` — confirm NAMESPACE is complete
- Run `devtools::check()` — target: 0 errors, 0 warnings, 0 notes (or document any unavoidable notes)

## Testing (`tests/testthat/test-mod_data_explorer.R`)
- CTF toggle returns `ctfdata_list` data (not `rawdata_list`)
- Raw Values toggle returns `rawdata_list` data
- Filtered table respects `year_range` bounds
- Filtered table respects `primary_iso` and `benchmark_isos`
- Download handler produces a non-empty CSV
- Dynamic subcluster selector updates correctly when cluster changes
- Table does not crash when filtered data is empty

## Final Validation Checklist
- [ ] `devtools::check()` — 0 errors, 0 warnings
- [ ] All `testthat` tests pass
- [ ] `run_cgjrapp()` launches cleanly from a fresh R session
- [ ] All 7 tabs render without error
- [ ] Navigating between tabs preserves sidebar state
- [ ] Plots render in both `"evolution"` and `"line"` modes
- [ ] Data explorer exports a valid CSV
- [ ] No hardcoded paths (all via `system.file()`)
