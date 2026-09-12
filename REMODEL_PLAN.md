# cgjrapp Fresh Build Plan v3 — execution plan for Claude Code

**Supersedes v2.** v2 was correct on substance but ordered as horizontal
layers (finish the whole data layer, then the whole methodology layer, then
the UI). This version keeps every v2 decision but reorders the work into
vertical slices — one real leaf working end-to-end first, then generalized
outward — and is written to be handed directly to Claude Code as a build
order, not just a design doc.

This is a **fresh build, not a remodel.** Nothing in the old `cgjrapp`
`R/` or `tests/` directories is adapted in place. Old files are reference
material for behavior/UX to preserve (or explicitly not repeat — see
"Anti-patterns" below) until the new module that supersedes them is working,
at which point the old file is deleted in the same commit. `cliarappak` is
the methodology and styling source of truth — its scoring, thresholding,
and plotting logic gets ported deliberately and checked against its own
output, never reimplemented from memory. `cgjrdata` is an unmodified
dependency for this plan; it already ships the six tidy objects this plan
is built against (`cgjr_taxonomy`, `cgjr_crosswalk`, `cgjr_ctf`,
`cgjr_scores`, `cgjr_raw`, `wbcountries`).

---

## Non-negotiable constraints — the acceptance bar for every slice

1. **Taxonomy-shape agnostic, within the current 3-level cap.** No file
   anywhere may hardcode a cluster, subcluster, or leaf name, count, or
   nesting depth. Every label the UI shows comes from `cgjr_taxonomy`'s
   `cluster_name` / `subcluster_name` / `sub_subcluster_name` columns;
   every ordering comes from its `*_num` columns; a leaf's depth is read
   from whether `sub_subcluster` is `NA` for that row, never assumed. If
   the crosswalk reshapes — more clusters, more subclusters, the extra
   nesting level moving off PFM to a different branch, a branch
   disappearing — the app must reflect it with zero code changes, as long
   as the tibble schemas and the methodology below stay the same.
   **Explicitly out of scope (decided, not an oversight):** a genuine
   fourth nesting level (a `sub_subsubcluster`). `cgjr_taxonomy` is a
   fixed-width 3-level schema (nine named columns, not a variable-depth
   representation) and Slice 2's accordion walker is written as a fixed
   three-step descent to match it. Going deeper than that would require a
   schema change to `cgjr_taxonomy` itself (out of scope — `cgjrdata` is
   an unmodified dependency for this plan) and a rewrite of the accordion
   walker into something genuinely recursive. Confirmed with Ify
   (2026-09-08) that this is an acceptable limitation to accept now rather
   than design around pre-emptively — revisit only if a fourth level
   becomes a real near-term need.
2. **Every accessor is parameterized**, never leaf-specific: functions take
   `leaf` / `node` / `node_level` / `ctf_type` / `unit_level` / `unit_code`
   as arguments. A function that only works for one leaf because its name
   is written into the function body violates this even if it's "just for
   now."
3. **Empty `(leaf, ctf_type)` fails clean.** Check `n_inputs == 0` on the
   guaranteed-present scaffolded row and render "no data available" — never
   an error, never a hidden/missing row, never a check done once per leaf
   instead of per `(leaf, ctf_type)`.
4. **Indicator-level CTF values are never recomputed**, only filtered and
   plotted as they come out of `cgjr_ctf` — confirmed this is exactly what
   `cliarappak` does with `cliaretl`'s values too (rename to `dtf`, rename
   to `var`, plot; never reassigned). Only composite/family scores and
   rank/status classification are computed live, per selection, by the app.
5. **The live methodology matches `cliarappak` exactly**: for a given base
   unit + comparator selection — (a) drop indicators 100%-NA for the base
   unit, (b) drop zero-variance indicators across the selection (excluding
   `_avg` columns), (c) percent-rank what's left within the selection only
   (never the global population) into Weak/Emerging/Strong bands at
   Default (25/50) or Terciles (33/66) cutoffs, (d) average the same
   filtered set for the family/composite score. This is a deliberate port
   of `def_quantiles()` / `def_quantiles_dyn()` / `compute_family_average_app()`,
   checked against `cliarappak`'s own output on a fixed test input — not an
   independent reimplementation.
6. **`cgjr_scores` is not the interactive data source.** Composite scores
   shown on screen are always the live per-selection computation (point 5).
   `cgjr_scores` is retained only as an optional, clearly-labeled
   "precomputed reference" export on the Data Download tab — it will not
   match the live numbers for non-default selections by design (median
   cross-country aggregation, no selection-dependent exclusion), and that's
   expected, not a bug.
7. **Zone/threshold styling and plot mechanics are ported from
   `cliarappak`'s `fct_plots.R`**, not reinvented — same colors, same
   static vs. dynamic chart construction.

---

## What `cgjrdata` actually ships (confirmed against the built `.rda` files)

| Object | Grain | Key columns |
|---|---|---|
| `cgjr_taxonomy` | 14 rows (10 two-level leaves + 4 PFM sub-subclusters) | `cluster`, `cluster_num`, `cluster_name`, `subcluster`, `subcluster_num`, `subcluster_name`, `sub_subcluster`, `sub_subcluster_num`, `sub_subcluster_name` — the last three `NA` for non-PFM leaves |
| `cgjr_crosswalk` | indicator × leaf (~110 rows) | `leaf`, `indicator`, `variable`, `source`, plus `cliaretl` eligibility flags (`in_cliaretl`, `in_dynamic_panel`, `in_static_panel`, `dynamic_eligible`, `static_eligible`, `cliaretl_status`) |
| `cgjr_ctf` | unit × year × `ctf_type` × leaf × indicator (258,230 rows; dynamic 232,260 / static 25,970) | scaffolded — every leaf has a row per `ctf_type` even when `n_inputs == 0` |
| `cgjr_scores` | unit × year × `ctf_type` × `node_level` (subcluster/sub_subcluster/cluster/overall) | reference-only per constraint 6 above |
| `cgjr_raw` | country × year × leaf × indicator, no `ctf_type`, no group rows | by design — no regional median at raw grain |
| `wbcountries` | unchanged | — |

Structural facts that drove the constraints above: emptiness is scoped to
`(leaf, ctf_type)`, not `leaf` (`budget_cycle_and_fiscal_planning` is
static-only: 24 static-eligible indicators, 0 dynamic; the other three PFM
sub-subclusters are empty in both); region/income rows are a `median`
computed independently at both the `cgjr_ctf` and `cgjr_scores` grain, so
they won't arithmetically reconcile with each other (expected); the rollup
math is externally verified against `cliaretl::compute_family_average(require_complete = FALSE)`
cell-for-cell for the six leaves whose indicator set equals a CLIAR
`family_var` (`qcheck/qa-report.md`) — what's *not* verified there is the
app-level selection-dependent exclusion step, which is this plan's job.
Deliberate taxonomy departures from CLIAR, relevant to the report prompt
(Slice 5): `justice_and_rule_of_law` omits `wjp_rol_8_2` (16 of 17
`vars_leg`); `political_institutions_and_social_cohesion` merges `vars_pol`
+ `vars_social`; `social_cohesion_norms_and_cooperation` is a 5-indicator
subset of `vars_pol`.

---

## Build order — vertical slices

### Slice 0 — prove the pipeline on one real leaf

Pick `justice_and_rule_of_law` (2-level, both `ctf_type`s populated, 16
indicators). Build only what this slice needs, but build it *generic*:

- `R/data_access.R`: `filter_unit_data(leaf, ctf_type, units, year_range)`
  against `cgjr_ctf` — takes `leaf` as an argument; a dev script filters
  `cgjr_taxonomy` down to one row and passes that leaf's key in, rather
  than the function knowing which leaf it is.
- `R/utils_family.R`: port `def_quantiles()` / `def_quantiles_dyn()` /
  `compute_family_average_app()` against the long `cgjr_ctf` shape. Write
  the numeric-parity test now — same base unit + comparator set fed to
  both `cliarappak`'s original function and this port, assert equal output
  — before trusting it inside any reactive.
- `R/plotting.R`: port the relevant piece of `cliarappak::fct_plots.R`
  (`static_plot()` / `static_plot_dyn()` / `trends_plot()`) plus zone
  colors from `utils_plotting.R`'s `CAPACITY_ZONES`/status palette.
- A minimal `mod_benchmark_dashboard.R` rendering exactly one leaf card
  (static + dynamic toggle, real plot, real composite score) inside a
  `bslib::accordion()` shell, with `base_country`/`comparators` as
  hardcoded reactive values at the very top of a dev script only — never
  inside the functions themselves.

**Exit criterion:** one correct, real leaf card renders in the browser,
and the parity test passes.

### Slice 1 — generalize data access + methodology

Drop the leaf filter. Extend `filter_unit_data()` and `utils_family.R`'s
functions to run across every row of `cgjr_taxonomy` — this should mostly
mean *removing* the dev-time filter, not rewriting the functions, since
they were parameterized from the start. Re-run the parity test across all
six leaves the QA report validated.

### Slice 2 — generalize the accordion

Accordion generation from the full `cgjr_taxonomy`: `group_by(cluster_num,
cluster, cluster_name)` → nested `group_by(subcluster_num, ...)` → descend
one more level only where `sub_subcluster` isn't `NA`. Exercise PFM (the
one branching case) and at least one fully-empty-in-both-`ctf_type`s
sub-subcluster (`domestic_revenue_mobilization`) as explicit test fixtures.
No per-cluster or per-subcluster UI files — one generic renderer walking
the taxonomy.

### Slice 3 — Country Selection tab (Tab 2)

Port `cliarappak::mod_benchmark.R`'s selection UI/logic (not its plots)
into `mod_country_selection.R`. One comparator picker over
`distinct(cgjr_ctf, unit_level, unit_code, unit_name)` — countries,
regions, and income groups as equal citizens. Returns a `bench`-style
reactive list; wire it into Tab 3 in place of Slice 0's hardcoded values.

### Slice 4 — caching

`shiny::bindCache()` on the live per-selection computation from
`utils_family.R`, keyed on the full selection signature (primary unit,
comparator units, year range, threshold mode, `ctf_type`, leaf) — same
mechanism already used in the old `mod_detail.R`. Optionally pre-warm the
cache for the default view (base country vs. its own region/income group,
no custom comparators).

### Slice 5 — Data Download & Report tabs

Rebuild table exports against the generalized Slice 1 accessors, exposing
both the live composite score and the `cgjr_scores` reference export,
clearly labeled as different (constraint 6). Rewrite `utils_report.R`'s
LLM system prompt for the current taxonomy and to state the CLIAR
departures above explicitly, rather than implying a cleaner
correspondence than exists.

### Slice 6 — Welcome tab

Carried over unchanged (`mod_welcome`) — the only tab not rebuilt.

**Cleanup is continuous, not a final phase.** The moment a slice's new
module supersedes an old file, delete the old file in the same commit as
its replacement — `mod_cluster_factory.R` and `test-mod_cluster.R` can go
immediately (confirmed dead code, unreferenced in `run_cgjrapp.R`);
`mod_detail.R`, `mod_overview.R`, `utils_data.R`'s old functions, and the
old zone code in `utils_plotting.R` go as each is superseded in Slices
0–2; `test-schema.R` is rewritten against the tidy shape (column
contracts, taxonomy row counts, scaffolding completeness) rather than
patched.

---

## Anti-patterns — things the old app did that must not recur

- Hardcoded display-name lookup tables (`CLUSTER_DISPLAY_NAMES`,
  `SUBCLUSTER_DISPLAY_NAMES`) — use `cgjr_taxonomy`'s `*_name` columns.
- Assuming exactly two levels of nesting anywhere (`purrr::imap()` over a
  cluster's children treating every child as a leaf tibble).
- "First subcluster as proxy" shortcuts for cluster-level aggregates.
- A leaf-specific hardcoded UI file, even temporarily — Slice 0's one
  leaf-card must come from the same generic renderer Slice 2 scales up,
  filtered down for dev speed, not a bespoke file.

---

## Open items to flag, not silently decide

- **Even-years-only dynamic gating**: `cliarappak` restricts dynamic-panel
  comparisons to even years for strict CLIAR parity; `cgjrapp` has
  historically used the full year range. Ask before Slice 2/3 whether the
  new dashboard should match `cliarappak` here or keep full years —
  this wasn't settled by the `cgjrdata` rewrite, just newly documented as
  a known difference.
- **Data Download "Scores" export** exposes both live and `cgjr_scores`
  values per constraint 6 — confirm the labeling reads clearly to an end
  user before shipping Slice 5.

---


> Read `copilot_logs/CONTEXT_FOR_CGJRAPP.md` and `PLAN.md` in the
> `cgjrdata` repo (https://github.com/WB-PIDA-Data-Science-Shop/cgjrdata), and `R/fct_quantiles.R`, `R/fct_family.R`, and the
> `static_plot()`/`static_plot_dyn()` sections of `R/fct_plots.R` in the
> `cliarappak` repo (https://github.com/WB-PIDA-Data-Science-Shop/cliarappak), go ahead and study all of cliarappak thoroughly, then read this plan in full. Start on Slice 0 only:
> build `filter_unit_data()`, the ported `def_quantiles()`/
> `compute_family_average_app()` in `utils_family.R` with its parity test
> against `cliarappak`'s own output, and one working leaf card for
> `justice_and_rule_of_law` in a minimal accordion shell. Stop and check in
> once the parity test passes and the leaf card renders — don't proceed to
> Slice 1 until that's confirmed working.
 