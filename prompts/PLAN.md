# cgjrapp — Master Build Plan

## Overview

This document is the authoritative reference for the `cgjrapp` build. It describes the sprint sequence, the rationale behind key design decisions, the file map, and the deploy-test-debug cycle we follow throughout.

---

## Guiding Principles

1. **Code → Test → Deploy at each sprint.** Every sprint ends with a runnable app and a passing test suite. We never accumulate untested code across multiple sprints.
2. **No UI before the engine is tested.** Sprints 1–3 produce zero visible UI changes. The data and plotting layers are fully validated before any module code is written.
3. **One function, one responsibility.** `filter_cgjr_data()` filters. `plot_cgjr_master()` plots. Modules wire them together. Nothing else.
4. **Shared state flows down, never up.** Global sidebar controls live in `run_cgjrapp()` and are passed as reactive arguments to modules. Modules never own state that another module needs.
5. **`ggplot2` is the source of truth for plots.** `plotly::ggplotly()` wraps at render time. The core plot function returns a `ggplot` — always testable, always inspectable.

---

## Sprint Sequence

| Sprint | Prompt File | Key Output | Runnable? | Tests? |
|---|---|---|---|---|
| 1 | `cgjrapp_01-infrastructure.md` | Clean package shell, `DESCRIPTION`, `globals.R` | ✓ (home tab only) | Schema tests |
| 2 | `cgjrapp_02-data-helpers.md` | `utils_data.R` — filter + choice helpers | ✓ (home tab only) | Full helper tests |
| 3 | `cgjrapp_03-plot-engine.md` | `utils_plotting.R` — master plot function | ✓ (home tab only) | Full plot tests |
| 4 | `cgjrapp_04-welcome-overview.md` | `mod_welcome.R`, `mod_overview.R`, shared sidebar | ✓ (2 tabs live) | Module tests |
| 5 | `cgjrapp_05-cluster-factory.md` | `mod_cluster_factory.R` — 4 cluster tabs | ✓ (6 tabs live) | Factory tests |
| 6 | `cgjrapp_06-data-explorer-final.md` | `mod_data_explorer.R`, final checks | ✓ (7 tabs live) | Full suite + check() |

---

## File Map

```
R/
  run_cgjrapp.R          # Entry point, app assembly, shared sidebar
  globals.R              # NSE globalVariables() declarations
  utils_data.R           # filter_cgjr_data(), get_country_choices(), etc.
  utils_plotting.R       # plot_cgjr_master(), get_capacity_zones()
  mod_welcome.R          # Tab 1: Welcome
  mod_overview.R         # Tab 2: Overview (1+4 grid)
  mod_cluster_factory.R  # Tabs 3–6: Parameterised cluster modules
  mod_data_explorer.R    # Tab 7: Data Explorer

tests/testthat/
  test-utils_data.R
  test-utils_plotting.R
  test-mod_overview.R
  test-mod_cluster_factory.R
  test-mod_data_explorer.R

inst/
  www/
    styles.css
    cgjr_logo.jpg
  markdown/
    cgjr_home.md

prompts/
  cgjrapp_01-infrastructure.md
  cgjrapp_02-data-helpers.md
  cgjrapp_03-plot-engine.md
  cgjrapp_04-welcome-overview.md
  cgjrapp_05-cluster-factory.md
  cgjrapp_06-data-explorer-final.md
```

---

## Reactive State Design

```
run_cgjrapp()
│
├── [Top-level sidebar]
│     primary_iso       → selectInput
│     benchmark_groups  → selectizeInput  (precomputed aggregates)
│     peer_isos         → selectizeInput  (ad-hoc ISO3 peers)
│     year_range        → sliderInput
│     view_mode         → radioButtons    (evolution / line)
│
├── mod_welcome_server()         [no reactive args needed]
├── mod_overview_server()        [receives all 5 global reactives]
├── mod_cluster_server("c1", …)  [receives all 5 global reactives + local indicator]
├── mod_cluster_server("c2", …)  [same]
├── mod_cluster_server("c3", …)  [same]
├── mod_cluster_server("c4", …)  [same]
└── mod_data_explorer_server()   [receives primary_iso, benchmark_isos, year_range]
```

**Rule:** `view_mode` and `selected_indicator` are the only controls that can be considered local. `view_mode` is currently global (Overview sidebar) but could be made local per cluster in a future iteration.

---

## Tab Structure

| Tab # | Name | Module | Data Source |
|---|---|---|---|
| 1 | Home / Welcome | `mod_welcome` | `metadata_tbl` |
| 2 | Overview | `mod_overview` | `institutional_averages_tbl` |
| 3 | Institutional Environment | `mod_cluster_factory` ("institutional_environment") | `ctfdata_list$institutional_environment` |
| 4 | Political Institutions | `mod_cluster_factory` ("political_institutions") | `ctfdata_list$political_institutions` |
| 5 | Center of Government | `mod_cluster_factory` ("center_of_government") | `ctfdata_list$center_of_government` |
| 6 | Sectors / Service Delivery | `mod_cluster_factory` ("sectors_service_delivery") | `ctfdata_list$sectors_service_delivery` |
| 7 | Data Explorer | `mod_data_explorer` | `ctfdata_list` + `rawdata_list` |

---

## Key Design Decisions & Rationale

### Why `ggplot2` + `ggplotly()`, not native plotly?
`ggplot2` objects are inspectable in tests (check for layers, scales, data). Native plotly is opaque. We get tooltips from `ggplotly()` for free without sacrificing testability.

### Why a shared top-level sidebar?
The user should select their country once and see it reflected everywhere. Putting the primary country selector inside each module would require the user to re-select on every tab — a significant UX cost for a tool used by governance specialists doing country diagnostics.

### Why a factory pattern for clusters?
13 subclusters across 4 clusters. Writing 4 separate modules would mean 4× the code and 4× the bugs. The factory is parameterised by `cluster_name` — one module, one set of tests, four instantiations.

### Why sparse benchmark dots in evolution mode when range > 10 years?
With V-Dem data going back to 1990, plotting all years for 50+ benchmark countries produces a wall of overplotted dots. Sparse sampling (every 5 years) for benchmark countries, while keeping the primary country fully plotted, communicates trends without visual noise.

### Why precomputed aggregates in `cgjrdata`?
Computing regional/income averages at runtime inside the app would add latency and complexity. Pre-computing in `cgjrdata` means the app is a pure consumer — no aggregation logic to test or maintain.

---

## Prerequisites Before Sprint 1

1. **Testing prompt** — share your preferred testing protocol before any code is written
2. **Data contract validation** — run the following in console:
   ```r
   library(cgjrdata)
   names(institutional_averages_tbl)
   dplyr::glimpse(institutional_averages_tbl)
   names(ctfdata_list)
   purrr::map(ctfdata_list, names)
   names(metadata_tbl)
   names(ctfdata_list[[1]][[1]])
   institutional_averages_tbl |>
     dplyr::distinct(country_code, country_name) |>
     dplyr::filter(!stringr::str_detect(country_code, "^[A-Z]{3}$"))
   ```
