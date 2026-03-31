# Sprint 2: Data Helpers

## Objective
Build and thoroughly test the data access and filtering layer. This sprint produces **no UI**. It is complete when all data helpers have 100% test coverage and the data contract with `cgjrdata` is fully validated.

## Confirmed Data Contract
- `institutional_averages_tbl`: `country_code`, `country_name`, `year`, 4 cluster scores + `overall_score`. Year range 2013–2024. No aggregate rows.
- `ctfdata_list[[cluster]][[subcluster]]`: `country_code`, `country_name`, `year`, indicator columns…, `score`, `var_count`, `nonna_count`
- `regionctf_list[[cluster]][[subcluster]]`: `region_code`, `region`, `year`, indicator columns…, `score`  
  Region codes: `AFE`, `AFW`, `EAP`, `ECA`, `LAC`, `MENAAP`, `NAC`, `SAR`
- `incomectf_list[[cluster]][[subcluster]]`: `income_group`, `year`, indicator columns…, `score`  
  Income groups: `"High income"`, `"Low income"`, `"Lower middle income"`, `"Upper middle income"`
- `wbcountries`: `country_code`, `economy`, `income_group`, `region_code`, `region`, `lending_category` (218 rows)
- `aggregate_data_list`: currently NULL — do not use

## Tasks

### `R/utils_data.R`

#### `filter_country_data(data, primary_iso, peer_isos, year_range)`
- `data`: a country-keyed tibble (either `institutional_averages_tbl` or a subcluster tibble from `ctfdata_list`)
- `primary_iso`: single character ISO3 code for the focal country
- `peer_isos`: character vector of additional ISO3 peer country codes (length ≥ 0)
- `year_range`: integer vector of length 2: `c(min_year, max_year)`, inclusive
- **Returns:** filtered tibble with only `country_code %in% c(primary_iso, peer_isos)` and years in range
- **Adds column** `country_type`: `"primary"` for `primary_iso`, `"peer"` for all others

#### `get_aggregate_data(cluster, subcluster, region_codes, income_groups, year_range)`
- `cluster`: character — key from `names(ctfdata_list)`
- `subcluster`: character — key from `names(ctfdata_list[[cluster]])`
- `region_codes`: character vector of `region_code` values (length ≥ 0)
- `income_groups`: character vector of `income_group` values (length ≥ 0)
- `year_range`: integer vector of length 2
- **Returns:** a normalised tibble with columns `group_label`, `group_code`, `year`, `score`, `country_type`
  - `group_label`: human-readable name (e.g. `"East Asia & Pacific"`, `"High income"`)
  - `group_code`: code used for matching (e.g. `"EAP"`, `"High income"`)
  - `country_type`: `"region"` or `"income"` — both render as dashed lines
- **Logic:** bind rows from `regionctf_list[[cluster]][[subcluster]]` filtered by `region_codes` and `incomectf_list[[cluster]][[subcluster]]` filtered by `income_groups`; returns empty tibble if both vectors are empty

#### `get_indicator_choices(subcluster_tbl)`
- Takes a single subcluster tibble from `ctfdata_list`
- **Standard columns to exclude:** `country_code`, `country_name`, `year`, `score`, `var_count`, `nonna_count`
- Returns a named character vector of indicator column names only
- Used to populate the indicator selector dropdown in cluster modules

#### `get_country_choices()`
- Returns a named character vector of all countries in `institutional_averages_tbl`, sorted alphabetically
- Format: `c("Afghanistan" = "AFG", ...)` — uses `country_name` as label, `country_code` as value

#### `get_region_choices()`
- Returns a named character vector from `wbcountries` distinct `region_code`/`region` pairs
- Format: `c("Africa Eastern and Southern" = "AFE", ...)`

#### `get_income_choices()`
- Returns a named character vector of distinct `income_group` values from `wbcountries` (excluding NA)
- Format: `c("High income" = "High income", ...)` — value and label are the same string

## Testing (`tests/testthat/test-utils_data.R`)

### `filter_country_data()`
- Returns correct rows for known `primary_iso` and year range
- Assigns `country_type == "primary"` to primary, `"peer"` to peers
- Returns zero rows gracefully when country not in data (no error)
- Returns zero rows gracefully when year range has no coverage
- Handles `peer_isos = character(0)` — only primary country returned
- Year range is inclusive on both ends
- Adds `country_type` column to output
- Errors informatively when `data` missing `country_code` column
- Errors informatively when `data` missing `year` column
- No duplicate rows when `primary_iso` also appears in `peer_isos`

### `get_aggregate_data()`
- Returns rows for requested `region_codes`
- Returns rows for requested `income_groups`
- Returns both region and income rows when both requested
- `country_type` is `"region"` for region rows and `"income"` for income rows
- Returns empty tibble (not error) when both `region_codes` and `income_groups` are empty
- Year range filtering is applied correctly
- Returns correct `group_label` and `group_code` values

### `get_indicator_choices()`
- Returns only indicator columns (not `score`, `var_count`, `nonna_count`, etc.)
- Returns named character vector
- Returns `character(0)` when no indicator columns exist beyond standard set

### `get_country_choices()`
- Returns named character vector sorted alphabetically by name
- Values are ISO3 codes, names are country names

### `get_region_choices()` / `get_income_choices()`
- Both return named character vectors
- `get_region_choices()` has 8 entries (one per WB region)
- `get_income_choices()` has 4 entries (no NA)
- No overlap between region codes and income group values

## Deploy Check
At end of sprint: all tests pass, `devtools::check()` still clean. No UI changes.
