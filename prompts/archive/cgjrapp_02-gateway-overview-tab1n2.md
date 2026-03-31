# Sprint 2: Tab 1 (Welcome) & Tab 2 (Overview)

## 1. Objective
Finish the Welcome page and build the high-level benchmarking dashboard.

## 2. Tasks
- **Tab 1 (`R/mod_welcome.R`):**
  - Finish the existing implementation: Center DICE logo, finalize Governance GP headers.
  - Populate the "4 Cluster" descriptions using `cgjrdata::metadata_tbl`.
- **Tab 2 (`R/mod_overview.R`):**
  - **Sidebar:** Implement `radioButtons` for View Mode (Evolution vs Line), `selectInput` for Country, `selectizeInput` for Groups and Peers, and a `sliderInput` for Year Range.
  - **Main Area:** 1+4 Grid using `bslib::layout_column_wrap`.
    - Top: Overall Institutional CTF Plot.
    - Bottom: 2x2 grid for Cluster 1, 2, 3, and 4.
- **Reactive Logic:**
  - Ensure all 5 plots update simultaneously when any sidebar control is changed.
- **Testing:**
  - Verify that the `year_range` slider correctly updates the x-axis limits across all 5 plots.

  Is anything unclear with the prompt? What do you think about the design of thus far app? Does it need improvements? What would you do differently? 
