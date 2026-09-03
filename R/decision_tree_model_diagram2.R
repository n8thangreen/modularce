#' Generate Decision Tree Diagram
#' 
#' @param p_healthy Branch notation/label for healthy state
#' @param p_preclin Branch notation/label for pre-clinical state
#' @param p_disease Branch notation/label for disease state
#' @param file Optional filepath to save output plot (e.g. "plots/decision_tree.png")
#' @param safe_dir Optional directory restriction. Defaults to working directory.
#' @export
plot_decision_tree_diagram <- function(p_healthy = "&pi;<sub>t</sub>(1)",
                                       p_preclin = "&pi;<sub>t</sub>(2)",
                                       p_disease = "&pi;<sub>t</sub>(3)",
                                       file = NULL,
                                       safe_dir = getwd()) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE) || !requireNamespace("glue", quietly = TRUE)) {
    stop("Packages 'DiagrammeR' and 'glue' are required.")
  }
  
  tree_spec <- glue::glue("
digraph decision_tree {{
  graph [layout = dot, rankdir = LR, nodesep = 0.5, ranksep = 1.2]

  Root [label = 'Screening\\nEpisode']

  node [shape = circle, style = solid, fillcolor = white, width = 1.2]
  H [label = 'Healthy']
  P [label = 'Pre-clinical\\nDisease']
  D [label = 'Disease']

  node [shape = box, style = rounded, width = 1.8, fillcolor = white]
  FP   [label = 'False Positive\\nCost: 200 | Eff: 0\\n\\n-> State: Treated']
  TN   [label = 'True Negative\\nCost: 50 | Eff: 0\\n\\n-> State: Healthy']
  TP_P [label = 'True Positive\\n(Pre-clinical)\\nCost: 200 | Eff: 0\\n\\n-> State: Treated']
  FN_P [label = 'False Negative\\n(Pre-clinical)\\nCost: 50 | Eff: 0\\n\\n-> State: Pre_clinical_Disease']
  TP_D [label = 'True Positive\\n(Disease)\\nCost: 200 | Eff: 0\\n\\n-> State: Treated']
  FN_D [label = 'False Negative\\n(Disease)\\nCost: 50 | Eff: 0\\n\\n-> State: Disease']

  Root -> H [label = < <FONT POINT-SIZE='11'>{p_healthy}</FONT> >]
  Root -> P [label = < <FONT POINT-SIZE='11'>{p_preclin}</FONT> >]
  Root -> D [label = < <FONT POINT-SIZE='11'>{p_disease}</FONT> >]
  
  H -> FP [label = ' 0.10']
  H -> TN [label = ' 0.90']
  
  P -> TP_P [label = ' 0.70']
  P -> FN_P [label = ' 0.30']
  
  D -> TP_D [label = ' 1.00']
  D -> FN_D [label = ' 0.00']
}}
")
  
  model_plot <- DiagrammeR::grViz(tree_spec)
  
  if (!is.null(file)) {
    target_dir <- tryCatch({
      normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
    }, error = function(e) {
      stop("Invalid file path: directory does not exist or path is invalid.")
    })
    
    allowed_dir <- normalizePath(safe_dir, winslash = "/", mustWork = TRUE)
    target_dir_sep <- if (grepl("/$", target_dir)) target_dir else paste0(target_dir, "/")
    allowed_dir_sep <- if (grepl("/$", allowed_dir)) allowed_dir else paste0(allowed_dir, "/")
    
    if (!startsWith(target_dir_sep, allowed_dir_sep)) {
      stop("Invalid file path: path must be within the safe_dir.")
    }
    
    if (requireNamespace("DiagrammeRsvg", quietly = TRUE) && requireNamespace("rsvg", quietly = TRUE)) {
      raw_svg <- DiagrammeRsvg::export_svg(model_plot)
      rsvg::rsvg_png(charToRaw(raw_svg), file)
    } else {
      warning("Packages 'DiagrammeRsvg' and 'rsvg' are required to save diagram to file.")
    }
  }
  
  model_plot
}

plot_decision_tree_diagram(file = "plots/decision_tree_model2.png")
