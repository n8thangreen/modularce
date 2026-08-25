# ==============================================================================
# Plotting Script for Lin (2023) Screening Model Analysis
#
# Generates:
# 1. State Occupancy Stacked Bar Plots (2-yearly, 5-yearly, 10-yearly screening)
# 2. Cost-Effectiveness Frontier Plot across Multiple Screening Scenarios
#
# Uses the simplified S3 modular architecture from:
# scripts/Lin-(2023)-example-in-paper.R
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

# Source the main analysis script to load the model framework and parameters
source("scripts/Lin-(2023)-example-in-paper.R")

# Ensure plots directory exists
if (!dir.exists("plots")) {
  dir.create("plots")
}

# ==============================================================================
# 1. Helper Function: Extract Continuous State Occupancy Trace
# ==============================================================================

#' Extract and format the complete multi-interval cohort occupancy trace
#' from a CombinedModel result object
get_occupancy_trace <- function(combined_result) {
  # 1. Extract only the MarkovModel outputs from the sequence
  mm_outputs <- combined_result[sapply(combined_result, \(x) inherits(x, "output_MarkovModel"))]
  
  # 2. Combine all interval trace matrices into a single multi-year matrix
  trace_list  <- lapply(mm_outputs, \(x) x$trace)
  full_matrix <- do.call(rbind, trace_list)
  
  # 3. Pivot into tidy long format for ggplot2
  as_tibble(full_matrix) |>
    mutate(year = seq_len(nrow(full_matrix))) |>
    pivot_longer(
      cols      = -year,
      names_to  = "state",
      values_to = "occupancy"
    ) |>
    mutate(
      state     = factor(state, levels = states),
      rev_state = factor(state, levels = rev(states))
    )
}

# ==============================================================================
# 2. State Occupancy Stacked Bar Plots (20-Year Time Horizon)
# ==============================================================================

# Extract traces for each policy strategy
trace_2y  <- get_occupancy_trace(run_model(strategies[["2-Yearly Screening (10 rounds)"]]))
trace_5y  <- get_occupancy_trace(run_model(strategies[["5-Yearly Screening (4 rounds)"]]))
trace_10y <- get_occupancy_trace(run_model(strategies[["10-Yearly Screening (2 rounds)"]]))

#' Reusable plotting function for cohort state occupancy
plot_occupancy <- function(trace_data, title_text) {
  ggplot(trace_data, aes(x = year, y = occupancy, fill = rev_state)) +
    geom_col(position = "stack", width = 0.9) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 20, by = 2)) +
    scale_fill_brewer(palette = "Set2", name = "Health State") +
    labs(
      title = title_text,
      x     = "Year (Simulation Cycle)",
      y     = "Cohort Proportion"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position   = "right",
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold")
    )
}

# Generate and display plots
plot2  <- plot_occupancy(trace_2y,  "State Occupancy: 2-Yearly Screening (20-Year Horizon)")
plot5  <- plot_occupancy(trace_5y,  "State Occupancy: 5-Yearly Screening (20-Year Horizon)")
plot10 <- plot_occupancy(trace_10y, "State Occupancy: 10-Yearly Screening (20-Year Horizon)")

# Save occupancy plots
ggsave("plots/occupancy_plot_2y_screening.png",  plot2,  width = 8, height = 5, bg = "white")
ggsave("plots/occupancy_plot_5y_screening.png",  plot5,  width = 8, height = 5, bg = "white")
ggsave("plots/occupancy_plot_10y_screening.png", plot10, width = 8, height = 5, bg = "white")

cat("State occupancy plots saved to 'plots/' directory.\n")


# ==============================================================================
# 3. Multi-Scenario Cost-Effectiveness Frontier Plot
# ==============================================================================

# Define a grid of screening intervals (years between screens) and screening rounds
scenario_grid <- expand.grid(
  t_interval = c(2, 5, 10, 15, 20),
  n_screens  = c(1, 2, 3, 4, 5, 10)
)

# Run each scenario using the S3 modular framework
scenario_results <- lapply(seq_len(nrow(scenario_grid)), function(i) {
  t_val <- scenario_grid$t_interval[i]
  n_scr <- scenario_grid$n_screens[i]
  
  mm_t  <- MarkovModel(trans_mat, cost_vec, util_vec, mapping = mapping, n_cycles = t_val)
  chain <- do.call(CombinedModel, rep(list(dt, mm_t), n_scr))
  res   <- run_model(chain)
  
  tibble(
    t_interval = t_val,
    n_screens  = n_scr,
    cost       = get_costs(res),
    qalys      = get_effects(res)
  )
})

ce_data <- bind_rows(scenario_results) |>
  mutate(t_interval = factor(t_interval, levels = c(2, 5, 10, 15, 20)))

# Cost-Effectiveness scatter and trajectory plot
plot_ce <- ggplot(ce_data, aes(x = qalys, y = cost, color = t_interval, group = t_interval)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) +
  geom_text(aes(label = n_screens), vjust = -0.8, size = 3.2, show.legend = FALSE) +
  scale_color_viridis_d(name = "Screening Interval\n(Years)") +
  scale_x_continuous(breaks = seq(0, 16, by = 2)) +
  # scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    # title    = "Cost-Effectiveness of Screening Across Scenarios",
    # subtitle = "Connected lines show trajectories by screening interval; numbers indicate screening rounds",
    x        = "Total Health Gain (QALYs)",
    y        = "Total Expected Cost (£)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

plot_ce

ggsave("plots/ce-multiple-plot.png", plot_ce, width = 8, height = 5.5, bg = "white")
