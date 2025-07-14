################ NEED TO ADAPT TO MAKE A BIT MORE COMPLEX AND READ IN 
################ SOME OF THE INPUT FILES - THAT IS MORE REMINISCENT OF A 
################ THESE WERE DUMMY VALUES GPT CAME UP WITH
################ "PROPER" CEA RATHER THAN JUST DEFINING PARAMS IN THIS SCRIPT
################ can then add PSA too

####################### PARAMS ##################

age_start <- 25
age_end <- 100
n_cycles <- age_end - age_start

states <- c("Healthy", "Preclinical", "Clinical", "Cured", "Dead")
n_states <- length(states)

# Transition probabilities
p_incidence <- 0.01           # Healthy → Preclinical
p_progression <- 0.2          # Preclinical → Clinical
p_cure_clinical <- 0.4        # Clinical → Cured
p_cure_screened <- 0.8        # Preclinical (screened) → Cured
p_mortality <- 0.02           # Annual background mortality

# Screening
screen_start <- 50
screen_stop <- 70
screen_interval <- 5
screen_sens <- 0.85

# Utilities
utility <- c(Healthy = 1, Preclinical = 0.9, Clinical = 0.7, Cured = 0.95, Dead = 0)
disutility_screen <- 0.01

# Costs
cost <- c(Healthy = 0, Preclinical = 0, Clinical = 8000, Cured = 5000, Dead = 0)
cost_screen <- 100

# Discounting
discount_rate <- 0.03
discount_vector <- 1 / ((1 + discount_rate) ^ (0:(n_cycles - 1)))

####################### DECISION TREE #####################
screening_strategies <- list(
  NoScreening = list(screening = FALSE),
  Q5_50_70 = list(screening = TRUE, interval = 5, start = 50, stop = 70),
  Q2_45_75 = list(screening = TRUE, interval = 2, start = 45, stop = 75),
  OneTime_55 = list(screening = TRUE, interval = NA, start = 55, stop = 55),
  Annual_60_75 = list(screening = TRUE, interval = 1, start = 60, stop = 75)
)


############### MARKOV #######################
run_markov_strategy <- function(strategy) {
  pop <- matrix(0, nrow = n_cycles + 1, ncol = n_states)
  colnames(pop) <- states
  pop[1, "Healthy"] <- 1
  
  total_qalys <- 0
  total_costs <- 0
  
  for (t in 1:n_cycles) {
    age <- age_start + t - 1
    screen_now <- FALSE
    
    if (isTRUE(strategy$screening)) {
      if (age >= strategy$start && age <= strategy$stop) {
        if (is.na(strategy$interval)) {
          screen_now <- (age == strategy$start)
        } else {
          screen_now <- ((age - strategy$start) %% strategy$interval == 0)
        }
      }
    }
    
    current <- pop[t, ]
    next_state <- setNames(rep(0, n_states), states)
    
    # Preclinical transitions
    if (screen_now) {
      detected <- current["Preclinical"] * screen_sens
      undetected <- current["Preclinical"] * (1 - screen_sens)
      cured <- detected * p_cure_screened
      clinical_from_undetected <- undetected * p_progression
      
      next_state["Cured"] <- next_state["Cured"] + cured
      next_state["Clinical"] <- next_state["Clinical"] + clinical_from_undetected
      next_state["Preclinical"] <- next_state["Preclinical"] + undetected * (1 - p_progression)
    } else {
      progressed <- current["Preclinical"] * p_progression
      next_state["Clinical"] <- next_state["Clinical"] + progressed
      next_state["Preclinical"] <- next_state["Preclinical"] + current["Preclinical"] * (1 - p_progression)
    }
    
    # Clinical → Cured
    next_state["Cured"] <- next_state["Cured"] + current["Clinical"] * p_cure_clinical
    next_state["Clinical"] <- next_state["Clinical"] + current["Clinical"] * (1 - p_cure_clinical)
    
    # Healthy → Preclinical
    next_state["Preclinical"] <- next_state["Preclinical"] + current["Healthy"] * p_incidence
    next_state["Healthy"] <- next_state["Healthy"] + current["Healthy"] * (1 - p_incidence)
    
    # Mortality
    for (s in setdiff(states, "Dead")) {
      deaths <- next_state[s] * p_mortality
      next_state[s] <- next_state[s] - deaths
      next_state["Dead"] <- next_state["Dead"] + deaths
    }
    
    pop[t + 1, ] <- next_state
    
    year_qalys <- sum(current * utility)
    if (screen_now) year_qalys <- year_qalys - disutility_screen
    
    year_costs <- sum(current * cost)
    if (screen_now) year_costs <- year_costs + cost_screen
    
    total_qalys <- total_qalys + year_qalys * discount_vector[t]
    total_costs <- total_costs + year_costs * discount_vector[t]
  }
  
  return(list(qalys = total_qalys, costs = total_costs, trace = pop))
}


################## RUN SIMULATION ###########################

results <- data.frame(
  Strategy = names(screening_strategies),
  QALYs = NA,
  Costs = NA,
  ΔQALYs = NA,
  ΔCosts = NA,
  ICER = NA
)

baseline <- run_markov_strategy(screening_strategies$NoScreening)
results$QALYs[1] <- baseline$qalys
results$Costs[1] <- baseline$costs

for (i in 2:length(screening_strategies)) {
  strat <- names(screening_strategies)[i]
  res <- run_markov_strategy(screening_strategies[[strat]])
  
  results$QALYs[i] <- res$qalys
  results$Costs[i] <- res$costs
  results$ΔQALYs[i] <- res$qalys - baseline$qalys
  results$ΔCosts[i] <- res$costs - baseline$costs
  results$ICER[i] <- results$ΔCosts[i] / results$ΔQALYs[i]
}

print(results)

