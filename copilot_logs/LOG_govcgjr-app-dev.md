# Task Log: govcgjr-app-dev

## Task Overview

- **Task name:** govcgjr-app-dev
- **Description:** Create a Shiny app using the `cgjrdata` R package to deliver a dashboard supplementing the country growth and jobs institutional chapter's guidance note.
- **Initialized:** 2026-03-24

---

## Initial Context

### Package: `cgjrapp`
- R package under development at `~/GitProjects/cgjrapp`
- Entry point: `run_cgjrapp()` in `R/run_cgjrapp.R`
- Uses `bslib` (v0.9.0) for UI, `shiny` for server logic, `thematic` for theming
- Static assets in `inst/www/` (logo: `cgjr_logo.jpg`, CSS: `styles.css`)
- Markdown content in `inst/markdown/cgjr_home.md`
- `DESCRIPTION` is still a placeholder — needs updating

### Package: `cgjrdata` (upstream data dependency)
- Hosted at: https://github.com/WB-PIDA-Data-Science-Shop/cgjrdata
- Requires `cliaretl` (internal WB package) as upstream dependency
- Exports **four lazy-loaded objects:**
  | Object | Description |
  |---|---|
  | `rawdata_list` | Nested list of raw un-normalised source panels, keyed by cluster → subcluster |
  | `ctfdata_list` | Nested list of Closeness-to-Frontier (CTF) scores (0–1), with `score`, `var_count`, `nonna_count` columns |
  | `metadata_tbl` | One row per indicator: names, descriptions, sources, cluster/subcluster, benchmarking flags |
  | `institutional_averages_tbl` | Primary overview dataset: one row per `country_code × country_name × year`, with 4 cluster scores + overall score |

- **CGJR Analytical Framework:** 135 indicators across 4 clusters and 13 subclusters:
  | # | Cluster | Subclusters |
  |---|---|---|
  | 1 | Institutional Environment | Degree of Integrity, Transparency & Accountability, Justice & Rule of Law, Social Cohesion Norms & Cooperation |
  | 2 | Political Institutions | Political Institutions |
  | 3 | Center of Government | Public Financial Management, Public Sector HRM, Digital & Data |
  | 4 | Sectors / Service Delivery | Business Environment, Service Delivery, SOE Corporate Governance, Labor & Social Protection, Energy & Environment |

- **Score aggregation logic:**
  1. Subcluster score = row mean of CTF indicator columns
  2. Cluster score = mean of subcluster scores
  3. Overall score = mean of four cluster scores

- **Data sources:** V-Dem, WJP, PEFA, RISE, OECD PMR/EPL, Fraser, Heritage, WDI/WBL/GTMI

### Initial Fixes Applied (before task log)
- `navbar_options()` and `font_google()` namespaced to `bslib::`
- `addResourcePath` updated to use `system.file()` for package-safe paths
- Logo corrected from `.png` → `.jpg`
- CSS and markdown paths updated to `system.file()`
- Trailing comma and missing `...` in function signature fixed

---

## Progress Log

### 2026-03-24 — Task initialised
- Reviewed `run_cgjrapp.R` and `cgjrdata` repo README
- Created task log and `.current_task` marker

### 2026-03-24 — Data contract validated
- Ran full data contract exploration query; results locked in below

---

## Data Contract (confirmed 2026-03-24)

### `institutional_averages_tbl`
- **Dimensions:** 2,796 rows × 8 cols
- **Year range:** 2013–2024 (12 years — just above 10-yr sparse-dot threshold)
- **Columns:** `country_code`, `country_name`, `year`, `institutional_environment_score`, `political_institutions_score`, `center_of_government_score`, `sectors_service_delivery_score`, `overall_score`
- **Aggregate rows:** None in this table — aggregates live in separate lists (see below).

### Aggregate data — separate lists
| Object | Key column(s) | Values |
|---|---|---|
| `regionctf_list` | `region_code`, `region` | AFE, AFW, EAP, ECA, LAC, MENAAP, NAC, SAR |
| `regionrawdata_list` | `region_code`, `region` | same |
| `incomectf_list` | `income_group` | High income, Low income, Lower middle income, Upper middle income |
| `incomerawdata_list` | `income_group` | same |
| `aggregate_data_list` | — | Currently NULL/empty — reserved |
| `wbcountries` | `country_code`, `economy`, `income_group`, `region_code`, `region`, `lending_category` | 218 rows — country→region/income lookup |

### Key design implications from aggregate structure
- Aggregates are **separate lists**, not rows in country data — `filter_cgjr_data()` handles country data only
- New helper `get_aggregate_data()` fetches from `regionctf_list`/`incomectf_list` by selected codes
- `country_type` values: `"primary"`, `"peer"`, `"region"`, `"income"` — both region and income render as dashed lines
- `wbcountries` drives sidebar region/income selectors
- `aggregate_data_list` is NULL now — design must not depend on it

### `ctfdata_list` — cluster/subcluster keys (snake_case)
```
institutional_environment
  ├── degree_of_integrity
  ├── transparency_and_accountability
  ├── justice_and_rule_of_law
  └── social_cohesion_norms_and_cooperation

political_institutions
  └── political_institutions

center_of_government
  ├── public_financial_management
  ├── public_sector_hrm
  └── digital_and_data

sectors_service_delivery
  ├── business_environment
  ├── service_delivery
  ├── soe_corporate_governance
  ├── labor_and_social_protection
  └── energy_and_environment
```

### Subcluster tibble standard columns (to EXCLUDE from indicator choices)
`country_code`, `country_name`, `year`, `score`, `var_count`, `nonna_count`

### Example subcluster indicator columns (`degree_of_integrity`)
`wjp_rol_2`, `vdem_core_v2x_pubcorr`, `vdem_core_v2x_execorr`, `vdem_core_v2lgcrrpt`, `wjp_rol_6_2`

### `metadata_tbl` — 21 columns
Key columns: `var_name`, `variable`, `description`, `description_short`, `source`, `cluster`, `cluster_num`, `subcluster`, `subcluster_num`

### Cluster key mapping (snake_case → Title Case)
| `ctfdata_list` key | `metadata_tbl` cluster label |
|---|---|
| `institutional_environment` | `Institutional Environment` |
| `political_institutions` | `Political Institutions` |
| `center_of_government` | `Center of Government` |
| `sectors_service_delivery` | `Sectors / Service Delivery` |

### Key design implications
- Cluster factory label mapping must use the table above (cannot simply `tools::toTitleCase` due to `/`)
- Year range slider: min = 2013, max = 2024; default = c(2013, 2024)
- Sparse benchmark dots in evolution mode will trigger (12 years > 10)
- `get_aggregate_choices()` split into `get_region_choices()` and `get_income_choices()` — both sourced from `wbcountries`
- Region/income subcluster data uses `region_code`/`income_group` NOT `country_code` — plotting function must handle both key types

---

## To Do List

- [ ] Update `DESCRIPTION` (title, description, authors, license, imports)
- [ ] Add `cgjrdata` as a dependency (once access confirmed)
- [ ] Design dashboard panel structure (Home, Overview, Clusters, Country Profile, Metadata?)
- [ ] Implement Overview panel using `institutional_averages_tbl`
- [ ] Implement cluster/subcluster drill-down panels using `ctfdata_list`
- [ ] Country-level profile panel
- [ ] Metadata/indicator reference panel using `metadata_tbl`
- [ ] Refine UI/UX (styling, filters, interactivity)
- [ ] Write tests

---

## Decisions & Assumptions

- App is structured as an R package (`cgjrapp`) with `run_cgjrapp()` as entry point
- UI built with `bslib::page_navbar()` + `bslib` components
- `cgjrdata` is the sole data dependency — no direct DB or API calls in the app
- Paths to static assets use `system.file()` for portability

---

## Dependencies

| Package | Role |
|---|---|
| `shiny` | Core app framework |
| `bslib` | UI layout and theming |
| `thematic` | Auto-theming for ggplot2/etc. |
| `cgjrdata` | Data (to be added formally) |
