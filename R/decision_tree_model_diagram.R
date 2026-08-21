#' Generate Decision Tree Diagram
#' 
#' @param p_healthy Initial probability of starting healthy
#' @param p_preclin Initial probability of starting pre-clinical
#' @param p_disease Initial probability of starting disease
#' @param file Optional filepath to save output plot (e.g. "plots/decision_tree_model.png")
#' @export
plot_decision_tree_diagram <- function(p_healthy = 0.80,
                                        p_preclin = 0.15,
                                        p_disease = 0.05,
                                        file = NULL) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE) || !requireNamespace("glue", quietly = TRUE)) {
    stop("Packages 'DiagrammeR' and 'glue' are required to plot decision tree diagrams.")
  }

  tree_spec <- glue::glue("
digraph decision_tree {{
  graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 1.2]

  Root [label = 'Treatment A\\n(Cohort)']

  node [shape = circle, style = solid, fillcolor = white, width = 1.2]
  H [label = 'Healthy']
  P [label = 'Pre-clinical\\nDisease']
  D [label = 'Disease']

  node [shape = box, style = rounded, width = 1.8, fillcolor = white]
  FP [label = 'False Positive\\nCost: 200 | Eff: 0\\n\\n-> State: Treated']
  TN [label = 'True Negative\\nCost: 50 | Eff: 0\\n\\n-> State: Healthy']
  TP_P [label = 'True Positive\\n(Pre-clinical)\\nCost: 200 | Eff: 0\\n\\n-> State: Treated']
  FN_P [label = 'False Negative\\n(Pre-clinical)\\nCost: 50 | Eff: 0\\n\\n-> State: Pre_clinical_Disease']
  TP_D [label = 'True Positive\\n(Disease)\\nCost: 200 | Eff: 0\\n\\n-> State: Treated']
  FN_D [label = 'False Negative\\n(Disease)\\nCost: 50 | Eff: 0\\n\\n-> State: Disease']

  Root -> H [label = ' <<p_healthy>>']
  Root -> P [label = ' <<p_preclin>>']
  Root -> D [label = ' <<p_disease>>']
  
  H -> FP [label = ' 0.10']
  H -> TN [label = ' 0.90']
  
  P -> TP_P [label = ' 0.70']
  P -> FN_P [label = ' 0.30']
  
  D -> TP_D [label = ' 1.00']
  D -> FN_D [label = ' 0.00']
}}
", .open = "<<", .close = ">>")

  model_plot <- DiagrammeR::grViz(tree_spec)

  if (!is.null(file)) {
    if (requireNamespace("DiagrammeRsvg", quietly = TRUE) && requireNamespace("rsvg", quietly = TRUE)) {
      raw_svg <- DiagrammeRsvg::export_svg(model_plot)
      rsvg::rsvg_png(charToRaw(raw_svg), file)
    } else {
      warning("Packages 'DiagrammeRsvg' and 'rsvg' are required to save diagram to file.")
    }
  }

  model_plot
}
