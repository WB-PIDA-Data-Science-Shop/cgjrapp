# Sprint 5: Cluster & Subcluster Factory (Tabs 3–6)

## Objective
Build the generalized module factory that produces four identical-in-structure but data-distinct cluster tabs (Tabs 3–6). Each cluster tab contains dynamic pill sub-tabs for every subcluster. This sprint is complete when all four cluster tabs render, react to both shared and local controls, and factory tests pass.

## Context
- 4 clusters, 13 subclusters total (see `names(ctfdata_list)` and `purrr::map(ctfdata_list, names)`)
- Shared state (primary country, peers, year range) flows DOWN from `run_cgjrapp()` — same reactive arguments as Sprint 4
- Local state (indicator selection, view mode override) lives INSIDE each cluster module — does NOT need to persist across tabs
- The factory pattern means we write `mod_cluster_ui()` and `mod_cluster_server()` ONCE and instantiate them 4 times

## State Design Summary

| Control | Scope | Where it lives |
|---|---|---|
| `primary_iso` | Global | Top-level `run_cgjrapp()` sidebar |
| `benchmark_isos` (groups + peers) | Global | Top-level `run_cgjrapp()` sidebar |
| `year_range` | Global | Top-level `run_cgjrapp()` sidebar |
| `view_mode` | Global | Top-level `run_cgjrapp()` sidebar |
| `selected_indicator` | Local per subcluster pill | Inside cluster module |

## Tasks

### `R/mod_cluster_factory.R`

#### `mod_cluster_ui(id, cluster_name)`
- Uses `bslib::navset_card_pill()` to dynamically generate one pill tab per subcluster
- Subcluster names come from `names(cgjrdata::ctfdata_list[[cluster_name]])`
- Each pill contains `bslib::layout_sidebar()`:
  - **Sidebar (local):** `selectInput` for indicator selection — choices populated by `get_indicator_choices()` for that subcluster
  - **Main:** `plotly::plotlyOutput()` for the subcluster plot

#### `mod_cluster_server(id, cluster_name, primary_iso, benchmark_isos, year_range, view_mode)`
- Shared reactive args passed in: `primary_iso`, `benchmark_isos`, `year_range`, `view_mode`
- For each subcluster pill:
  - Reactive: filter `ctfdata_list[[cluster_name]][[subcluster]]` using `filter_cgjr_data()`
  - Reactive: selected indicator from local `selectInput`
  - Render: `plot_cgjr_master()` using `y_var = selected_indicator`
  - Wrapped with `plotly::ggplotly()`
- Use `purrr::walk()` or `lapply()` internally to register observers/renders for each subcluster without repetition

### Update `run_cgjrapp()`
- Use `purrr::walk()` to instantiate all 4 cluster modules:
```r
cluster_names <- names(cgjrdata::ctfdata_list)
purrr::walk(cluster_names, \(cn) {
  bslib::nav_panel(cn, mod_cluster_ui(cn, cn))
})
# and server side:
purrr::walk(cluster_names, \(cn) {
  mod_cluster_server(cn, cn,
    primary_iso    = reactive(input$primary_iso),
    benchmark_isos = reactive(c(input$benchmark_groups, input$peer_isos)),
    year_range     = reactive(input$year_range),
    view_mode      = reactive(input$view_mode)
  )
})
```
- Cluster tab labels should use human-readable names (map from `ctfdata_list` keys if needed)

## Testing (`tests/testthat/test-mod_cluster_factory.R`)
- `mod_cluster_ui()` generates the correct number of pill tabs (one per subcluster in that cluster)
- `mod_cluster_ui()` generates pill tabs with correct subcluster names
- `mod_cluster_server()` tested with `shiny::testServer()`:
  - Changing `primary_iso` reactive updates plot output
  - Changing `selected_indicator` (local input) updates plot output for that pill only
  - Changing `year_range` updates x-axis across all pills
  - Switching between pills maintains the same `primary_iso` selection
  - Empty data for a subcluster does not crash the module
- Factory produces exactly 4 cluster modules when called with `purrr::walk()` over `names(ctfdata_list)`

## Deploy Check
At end of sprint: `run_cgjrapp()` launches with all 6 tabs functional (Home, Overview, + 4 cluster tabs). Navigating between tabs preserves sidebar selections. All prior tests still pass.
