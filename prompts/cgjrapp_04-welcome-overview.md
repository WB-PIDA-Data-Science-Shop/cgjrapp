# Sprint 4: Welcome Tab & Overview Tab

## Objective
Build the first two visible tabs of the app. This sprint introduces **shared reactive state** — the global sidebar controls that persist across all tabs. It is complete when both tabs render correctly, react to sidebar inputs, and module tests pass.

## Context
- Tab 1 (Welcome) already has a shell. It needs content from `metadata_tbl`.
- Tab 2 (Overview) is the high-level benchmarking view using `institutional_averages_tbl`.
- **Critical design decision:** The four shared controls (primary country, benchmark groups, peers, year range) live in a **top-level sidebar in `run_cgjrapp()`**, not inside individual modules. This state is passed as `reactive` arguments into every module so that navigating between tabs never resets the user's selections.

## Shared Reactive State (in `run_cgjrapp()`)

The following inputs live at the app level (outside all modules):

| Input ID | Widget | Description |
|---|---|---|
| `primary_iso` | `selectInput` | Focal country — single ISO3 selection |
| `benchmark_groups` | `selectizeInput` | Precomputed regional/income aggregates |
| `peer_isos` | `selectizeInput` | Ad-hoc peer countries (real ISO3 codes) |
| `year_range` | `sliderInput` | Min/max year (integer range slider) |
| `view_mode` | `radioButtons` | `"evolution"` or `"line"` |

These are passed to modules as reactive expressions, e.g.:
```r
mod_overview_server("overview",
  primary_iso   = reactive(input$primary_iso),
  benchmark_isos = reactive(c(input$benchmark_groups, input$peer_isos)),
  year_range    = reactive(input$year_range),
  view_mode     = reactive(input$view_mode)
)
```

## Tasks

### `R/mod_welcome.R`
- `mod_welcome_ui(id)`: finalise layout
  - Centered CGJR logo (already partially done)
  - Governance GP header text
  - **4-cluster summary section:** Dynamically built from `cgjrdata::metadata_tbl` — extract distinct cluster names and descriptions; render as `bslib::card()` components (one per cluster)
  - Link to the guidance note (placeholder URL acceptable)
- `mod_welcome_server(id)`: minimal — no reactive inputs needed; reads `metadata_tbl` once on load

### `R/mod_overview.R`
- `mod_overview_ui(id)`: 
  - `bslib::layout_sidebar()` — sidebar is for display only (shared controls are top-level)
  - Main area: `bslib::layout_column_wrap()` 1+4 grid
    - Top row (full width): Overall Institutional CTF plot (`overall_score` from `institutional_averages_tbl`)
    - Bottom row (2×2): One plot per cluster (cluster score columns)
  - Each plot rendered as `plotly::plotlyOutput()`
- `mod_overview_server(id, primary_iso, benchmark_isos, year_range, view_mode)`:
  - All 5 plots update reactively whenever any input changes
  - Filter data using `filter_cgjr_data()`
  - Plot using `plot_cgjr_master()` wrapped with `plotly::ggplotly()`

### Update `run_cgjrapp()`
- Add top-level `bslib::layout_sidebar()` wrapping the `page_navbar`
- Add shared sidebar controls (see table above)
- Populate `selectInput` choices using `get_country_choices()` and `get_aggregate_choices()` from Sprint 2
- Wire modules

## Testing (`tests/testthat/test-mod_overview.R`)
- `mod_overview_server()` is tested with `shiny::testServer()`
- Changing `primary_iso` reactive causes all 5 plot outputs to update
- Changing `year_range` causes x-axis limits to update across all 5 plots
- Changing `view_mode` causes correct mode to be passed to `plot_cgjr_master()`
- Empty `benchmark_isos` does not crash the module
- `mod_welcome_ui()` renders without error
- Welcome tab cluster cards match the 4 cluster names in `metadata_tbl`

## Deploy Check
At end of sprint: `run_cgjrapp()` launches with a functional Welcome tab and Overview tab. Sidebar controls work. All prior tests still pass.
