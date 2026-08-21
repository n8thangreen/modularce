#

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


##########
# ce plot

x <- 
  rbind(analysis(screening_results2),
        analysis(screening_results5),
        analysis(screening_results10))

plot(NULL,
     xlab = "health", ylab = "cost",
     ylim = c(0, 800), xlim = c(0,10))

text(x = x[,2], y = x[,1], labels = c("2", "5", "10"))

##################
# list of looped scenarios

full_pop <- list()
plot_data <- list()

for (i in seq_along(all_results)) {
  markov_res <- all_results[[i]]$results[sapply(all_results[[i]]$results, \(x) inherits(x, "MarkovModel"))]
  pop <- lapply(markov_res, \(x) x$pop)
  list_of_slices <- lapply(pop, function(x) x[, , 1])
  full_pop[[i]] <- do.call(cbind, list_of_slices)
  
  plot_data[[i]] <-
    full_pop[[i]] |>
    melt(varnames = c("state", "cycle"),
         value.name = "occupancy") |> 
    mutate(label = factor(state, levels = 1:6,
                          labels = c("Healthy", "Pre_clinical_Disease", "Disease",
                                     "Treated", "Death_from_Disease", "Other_Cause_Death")),
           rev_label = factor(label, levels = rev(levels(label))))
}

occup_plot <- 
  ggplot(plot_data[[13]], aes(x = cycle, y = occupancy, fill = rev_label)) +
  geom_col(position = "stack") +
  theme_minimal()

occup_plot

x <- lapply(all_results, \(x) analysis(x$results))

xx <- do.call(rbind, x)

plot(NULL,
     xlab = "health", ylab = "cost",
     ylim = c(0, 1100), xlim = c(0,15))

text(x = xx[,2], y = xx[,1])  #, labels = c("2", "5", "10"))


df <- as.data.frame(xx)

# Add a grouping variable
points_per_group <- 6
n_group <- 5
df$point <- rep(c(1,2,3,4,5,10), times = n_group)
df$group <- rep(c(2,5,10,15,20), each = points_per_group)
df$group <- as.factor(df$group) # Treat group as a categorical variable

# Create the plot
ggplot(df, aes(x = V2, y = V1, group = group, color = group)) +
  geom_point(size = 3) +
  geom_path(linewidth = 1) + # Connect points within each group
  xlim(0, 15) + ylim(0, 1100) +
  labs(x = "Health",
       y = "Cost",
       color = "Time between\n screen") +
  theme_minimal()

