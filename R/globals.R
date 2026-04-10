# globals.R
# Suppress R CMD CHECK "no visible binding for global variable" notes
# arising from tidyverse non-standard evaluation (NSE) in dplyr and ggplot2.
# Add variable names here as new columns are referenced in the codebase.

utils::globalVariables(c(
  # --- institutional_averages_tbl columns ---
  "country_code",
  "country_name",
  "year",
  "overall_score",
  "institutional_environment_score",
  "political_institutions_score",
  "center_of_government_score",
  "sectors_service_delivery_score",

  # --- ctfdata_list subcluster tibble columns ---
  "score",
  "var_count",
  "nonna_count",

  # --- computed columns added by filter_country_data() / get_aggregate_data() ---
  "country_type",
  "group_label",
  "group_code",
  "display_label",
  ".data",

  # --- regionctf_list / regionrawdata_list columns ---
  "region_code",
  "region",

  # --- incomectf_list / incomerawdata_list columns ---
  "income_group",

  # --- wbcountries columns ---
  "economy",
  "lending_category",

  # --- ggplot2 aesthetic mappings ---
  "x",
  "y",
  "colour",
  "linetype",
  "alpha",
  "group",
  "fill",
  "shape",
  "status",
  "bench_label",
  "bench_shape",
  "year_chr",
  "text",

  # --- mod_overview.R module-level constants ---
  "OVERVIEW_CLUSTER_VARS",
  "CLUSTER_COLOURS",
  "STATUS_COLOURS",
  "BENCHMARK_SHAPES",

  # --- utils_plotting.R zone support ---
  "zone_type",
  "threshold_mode",
  "thresholds",
  "q1",
  "q2",
  "xmin",
  "xmax",
  "ymin_0",
  "ymax_1",
  "zone",
  "ymin",
  "ymax",

  # --- Feature: individual member-country dots ---
  "member_country_name",
  "member_text",
  "show_members",

  # - mod_overview_server globals
  "Name",
  "Score",
  "Year"
))
