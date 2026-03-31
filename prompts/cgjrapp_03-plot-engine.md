# Sprint 3: Master Plot Engine

## Objective
Build and thoroughly test the single plotting function that powers every chart in the app. This sprint produces **no UI**. It is complete when `plot_cgjr_master()` handles both view modes correctly and all plot tests pass.

## Context
- All plots in the app are produced by one function: `plot_cgjr_master()`
- The function returns a `ggplot2` object. It is wrapped with `plotly::ggplotly()` at the Shiny render step — NOT inside this function
- This separation keeps the core function cleanly testable
- Input data is always pre-filtered by `filter_cgjr_data()` (Sprint 2) before being passed to this function
- `country_type` column (added by `filter_cgjr_data()`) drives visual encoding: `"primary"` gets bold treatment, `"aggregate"` gets dashed lines, `"peer"` gets standard lines

## Capacity Zones
All plots share background shading for three capacity zones on the y-axis (CTF score 0–1):

| Zone | Range | Label |
|---|---|---|
| Low Capacity | 0.00 – 0.33 | "Low" |
| Emerging Capacity | 0.34 – 0.66 | "Emerging" |
| High Capacity | 0.67 – 1.00 | "High" |

Implemented with `geom_rect()` behind all other layers. Use muted, accessible fill colours.

## Tasks

### `R/utils_plotting.R`

#### `plot_cgjr_master(data, y_var, primary_iso, view_mode, year_range)`
- `data`: pre-filtered tibble with `country_code`, `country_name`, `year`, `country_type`, and the column named in `y_var`
- `y_var`: character — name of the column to plot on the y-axis (e.g. `"score"`, `"overall_score"`, or a specific indicator name)
- `primary_iso`: character — ISO3 of the focal country (for labelling)
- `view_mode`: one of `"evolution"` or `"line"`
- `year_range`: integer vector length 2 — sets explicit x-axis limits

**Mode: `"evolution"`**
- Background: capacity zone `geom_rect()` bands
- All non-primary countries: `geom_point()` with low alpha, small size, colour by `country_type` (peers grey, aggregates tinted)
- Primary country: bold connected path — `geom_line()` + `geom_point()` with larger size, distinct colour, on top
- If year range is wide (> 10 years), only plot benchmark dots at 5-year intervals to reduce clutter (primary country always plotted at all years)
- x-axis: `year`, y-axis: `y_var`, both axis limits set explicitly

**Mode: `"line"`**
- Background: capacity zone `geom_rect()` bands
- Primary country: solid line + points, distinct colour
- Peers (`country_type == "peer"`): solid lines, distinct colours (use a palette — max ~6 peers)
- Aggregates (`country_type == "aggregate"`): dashed lines, muted colour
- Legend is shown
- x-axis: `year`, y-axis: `y_var`

**Shared requirements:**
- `ggplot2::theme_minimal()` base, then custom theme tweaks consistent with `bslib` "litera" theme
- y-axis fixed to 0–1 (CTF scale)
- Informative axis labels and a subtitle showing the `y_var` name (human-readable, from `metadata_tbl` if available)
- No hard-coded country names or colours — everything driven by `country_type`

#### `get_capacity_zones()`
- Helper that returns the three `geom_rect()` layers as a list
- Called inside `plot_cgjr_master()` and also exported so tests can inspect it

## Testing (`tests/testthat/test-utils_plotting.R`)
- `plot_cgjr_master()` returns a `ggplot` object (not a plotly, not NULL)
- In `"evolution"` mode: plot contains at least one `GeomPoint` layer and one `GeomLine` layer
- In `"line"` mode: plot contains `GeomLine` layers
- Capacity zone rects are present in both modes (check for `GeomRect` layer)
- y-axis is fixed to 0–1
- Function does not error when `data` contains only the primary country (no peers)
- Function does not error when `data` is empty (zero rows) — returns a ggplot with an informative "no data" annotation
- In `"evolution"` mode with > 10 year range: benchmark dots are sparse (only at 5-year intervals)
- `get_capacity_zones()` returns a list of length 3

## Deploy Check
At end of sprint: both helper files (`utils_data.R`, `utils_plotting.R`) are complete and fully tested. `devtools::check()` clean. Still no UI changes — `run_cgjrapp()` launches the same home tab as before.
