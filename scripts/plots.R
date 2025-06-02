
library(ggplot2)
library(reshape2)


markov_res2 <- screening_results2[sapply(screening_results2, \(x) inherits(x, "MarkovModel"))]
markov_res5 <- screening_results5[sapply(screening_results5, \(x) inherits(x, "MarkovModel"))]
markov_res10 <- screening_results10[sapply(screening_results10, \(x) inherits(x, "MarkovModel"))]

pop2 <- lapply(markov_res2, \(x) x$pop)
pop5 <- lapply(markov_res5, \(x) x$pop)
pop10 <- lapply(markov_res10, \(x) x$pop)

list_of_slices2 <- lapply(pop2, function(i) i[, , 1])
full_pop2 <- do.call(cbind, list_of_slices2)

list_of_slices5 <- lapply(pop5, function(i) i[, , 1])
full_pop5 <- do.call(cbind, list_of_slices5)

list_of_slices10 <- lapply(pop10, function(i) i[, , 1])
full_pop10 <- do.call(cbind, list_of_slices10)


########
# plots

plot_data <-
  full_pop2 |>
  melt(varnames = c("state", "cycle"),
       value.name = "occupancy") |> 
  mutate(label = factor(state, levels = 1:6,
                        labels = c("Healthy", "Pre_clinical_Disease", "Disease",
                                   "Treated", "Death_from_Disease", "Other_Cause_Death")),
         rev_label = factor(label, levels = rev(levels(label))))

plot2 <- 
  ggplot(plot_data, aes(x = cycle, y = occupancy, fill = rev_label)) +
  geom_col(position = "stack") +
  # geom_vline(xintercept = c(0.5,10.5), linewidth = 1.4) +
  # geom_vline(xintercept = c(5.5,15.5), linewidth = 1) +
  theme_minimal()

plot_data <-
  full_pop10 |>
  melt(varnames = c("state", "cycle"),
       value.name = "occupancy") |> 
  mutate(label = factor(state, levels = 1:6,
                        labels = c("Healthy", "Pre_clinical_Disease", "Disease",
                                   "Treated", "Death_from_Disease", "Other_Cause_Death")),
         rev_label = factor(label, levels = rev(levels(label))))

plot10 <- 
  ggplot(plot_data, aes(x = cycle, y = occupancy, fill = rev_label)) +
  geom_col(position = "stack") +
  # geom_vline(xintercept = c(0.5,10.5), linewidth = 1.4) +
  # geom_vline(xintercept = c(5.5,15.5), linewidth = 1) +
  theme_minimal()

plot_data <-
  full_pop5 |>
  melt(varnames = c("state", "cycle"),
       value.name = "occupancy") |> 
  mutate(label = factor(state, levels = 1:6,
                        labels = c("Healthy", "Pre_clinical_Disease", "Disease",
                                   "Treated", "Death_from_Disease", "Other_Cause_Death")),
         rev_label = factor(label, levels = rev(levels(label))))

plot5 <- 
  ggplot(plot_data, aes(x = cycle, y = occupancy, fill = rev_label)) +
  geom_col(position = "stack") +
  # geom_vline(xintercept = c(0.5,10.5), linewidth = 1.4) +
  # geom_vline(xintercept = c(5.5,15.5), linewidth = 1) +
  theme_minimal()

ggsave(plot2, filename = "plots/occupancy_plot_2y_screening.png", bg = "white")
ggsave(plot5, filename = "plots/occupancy_plot_5y_screening.png", bg = "white")
ggsave(plot10, filename = "plots/occupancy_plot_10y_screening.png", bg = "white")
