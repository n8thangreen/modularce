library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(magrittr)

model_plot <- 
grViz("
digraph markov_model {
  # Graph layout parameters
  graph [layout = dot, rankdir = LR, overlap = false]

  # Node definitions - width increased slightly to fit extra text
  node [shape = box, style = rounded, fontname = Helvetica, width = 1.4, penwidth = 1.5]
  
  H [label = 'Healthy\\nCost: 0\\nQALY: 1.0']
  P [label = 'Pre-clinical\\nDisease\\nCost: 0\\nQALY: 0.8']
  D [label = 'Disease\\nCost: 0\\nQALY: 0.5']
  T [label = 'Treated\\nCost: 100\\nQALY: 0.9']
  
  # Force absorbing states to be aligned in the same right-hand column
  {
    rank = same;
    DD [label = 'Death\\n(Disease)\\nCost: 0\\nQALY: 0']
    OD [label = 'Death\\n(Other)\\nCost: 0\\nQALY: 0']
  }

  # Transition edges with probabilities
  H -> H [label = ' 0.90']
  H -> P [label = ' 0.05']
  H -> OD [label = ' 0.05']

  P -> P [label = ' 0.65']
  P -> D [label = ' 0.20']
  P -> DD [label = ' 0.10']
  P -> OD [label = ' 0.05']

  D -> D [label = ' 0.60']
  D -> T [label = ' 0.20']
  D -> DD [label = ' 0.20']

  T -> T [label = ' 0.95']
  T -> OD [label = ' 0.05']

  DD -> DD [label = ' 1.00']
  OD -> OD [label = ' 1.00']
}
")

model_plot %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png("plots/markov_model.png")  # Save as PNG
