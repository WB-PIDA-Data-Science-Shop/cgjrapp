# Sprint 3: The Automated Cluster Factory

## 1. Objective
Create a generalized module factory to generate Tabs 3-6 (Clusters) and their internal sub-tabs (Subclusters).

## 2. Tasks
- **Factory Module (`R/mod_cluster_factory.R`):**
  - Create `mod_cluster_ui(id, cluster_name)` and `mod_cluster_server(id, cluster_name)`.
  - **Navigation:** Use `navset_card_pill()` to dynamically create sub-tabs for every subcluster in `names(ctfdata_list[[cluster_name]])`.
  - **Layout:** Use `layout_sidebar()` within each pill.
  - **Sidebar:** Contains an **Indicator Selector** (dropdown of variables within that subcluster), the **View Mode** toggle, and a **Year Slider**.
  - **Main:** Render `plot_cgjr_master`.
- **Assembly:**
  - In `run_cgjrapp()`, use a functional approach (e.g., `purrr::walk`) to instantiate the 4 cluster modules in the top-level navbar.
- **Testing:**
  - Verify that switching between subcluster pills maintains the user's selected `primary_country` but allows for local indicator selection.


  Is anything unclear with the prompt? What do you think about the design of thus far app? Does it need improvements? What would you do differently? 
