library(DiagrammeR)
library(glue)

# 1. Initial probabilities (dynamically passed)
p_healthy <- 0.80
p_preclin <- 0.15
p_disease <- 0.05

# 2. Parameterized graph with Markov mapping
tree_spec <- glue("
digraph decision_tree {
  # Graph layout parameters
  graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 1.2]

  # Root Node
  node [shape = square, style = 'rounded,filled', fillcolor = '#EAEAEA', fontname = Helvetica, width = 1.2]
  Root [label = 'Treatment A\\n(Cohort)']

  # Chance Nodes
  node [shape = circle, style = solid, fillcolor = white, width = 1.2]
  H [label = 'Healthy']
  P [label = 'Pre-clinical\\nDisease']
  D [label = 'Disease']

  # Terminal Nodes (Test Outcomes, Costs, and Markov Mapping)
  # Width increased slightly to accommodate the mapping text
  node [shape = box, style = rounded, width = 1.8, fillcolor = white]
  
  FP [label = 'False Positive\\nCost: 200 | Eff: 0\\n\\n➔ State: Treated']
  TN [label = 'True Negative\\nCost: 50 | Eff: 0\\n\\n➔ State: Healthy']
  
  TP_P [label = 'True Positive\\n(Pre-clinical)\\nCost: 200 | Eff: 0\\n\\n➔ State: Treated']
  FN_P [label = 'False Negative\\n(Pre-clinical)\\nCost: 50 | Eff: 0\\n\\n➔ State: Pre_clinical_Disease']
  
  TP_D [label = 'True Positive\\n(Disease)\\nCost: 200 | Eff: 0\\n\\n➔ State: Treated']
  FN_D [label = 'False Negative\\n(Disease)\\nCost: 50 | Eff: 0\\n\\n➔ State: Disease']

  # ---------------------------------------------------
  # Edges from Root (Variable Probabilities)
  # ---------------------------------------------------
  Root -> H [label = ' <<p_healthy>>']
  Root -> P [label = ' <<p_preclin>>']
  Root -> D [label = ' <<p_disease>>']
  
  # ---------------------------------------------------
  # Edges for Test Outcomes (Fixed Probabilities)
  # ---------------------------------------------------
  H -> FP [label = ' 0.10']
  H -> TN [label = ' 0.90']
  
  P -> TP_P [label = ' 0.70']
  P -> FN_P [label = ' 0.30']
  
  D -> TP_D [label = ' 1.00']
  D -> FN_D [label = ' 0.00']
}
", .open = "<<", .close = ">>")

# 3. Render the tree
model_plot <- grViz(tree_spec)


model_plot %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png("plots/decision_tree_model.png")  # Save as PNG
