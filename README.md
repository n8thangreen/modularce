---
editor_options: 
  markdown: 
    wrap: 72
---

# modularce

> A Unified Framework for Multiple Component Cost-Effectiveness Models
> in R

## Overview

**modularce** provides a comprehensive, structured approach to building
cost-effectiveness models that combine multiple modeling components—such
as decision trees and Markov models. Rather than linking these models in
an ad-hoc fashion, this framework promotes modular, robust, and reusable
code design for health economic evaluations.

## Problem Statement

Health economic modelling and cost-effectiveness analysis (CEA)
frequently requires integrating multiple model structures: - **Decision
trees** for initial pathways or early decisions - **Markov models** for
long-term state transitions - Custom components for specialized analyses

However, these integrations are often implemented inconsistently,
leading to: - ❌ Less robust and error-prone code - ❌ Poor reusability
across projects - ❌ Difficult maintenance and validation - ❌ Reduced
transparency and reproducibility

## Solution

This framework presents several approaches to linking multiple model
components, ranging from simple to advanced implementations, with
different complexity-generalisability trade-offs. This code repository
should be used in conjunction with the accompanying manuscript : [To add
manuscript details once published]

## Key Features

-   🔧 **Modular design** — Build components independently and combine
    them seamlessly
-   📊 **Multiple approaches** — Choose the complexity level that suits
    your needs
-   🏥 **Evidence-based** — Grounded in real-life case studies from
    published research
-   📚 **Well-documented** — Comprehensive tutorials and practical
    examples
-   🔁 **Reusable** — Write once, apply across multiple analyses

## Getting Started

### Clone the repository:

``` bash
git clone https://github.com/n8thangreen/tb.outbreak.costing.git
```

### Basic Example

``` r

# Create your model components
decision_tree <- create_decision_tree(...)
markov_model <- create_markov_model(...)

# Link them together
combined_model <- link_models(decision_tree, markov_model)

# Run analysis
results <- run_analysis(combined_model)
```

## Folder structure

| Folder | Purpose |
|------------------------------------|------------------------------------|
| [`R`](R/) | Package source R functions for building and linking model components |
| [`man`](man/) | Function documentation (`.Rd` help files) |
| [`docs`](docs/) | Manuscript related documents |
| [`scripts`](scripts/) | Example analysis and model-running scripts |
| [`tests`](tests/) | Package unit tests |
| [`plots`](plots/) | Generated figures and visualisation outputs |
| [`.github`](.github/) | GitHub configuration and workflows |

## Files and scripts of interest

-   `R/config.R`: Package configuration and parameter loading logic
    (`load_parameters()` / `load_parameter()`).
-   `scripts/model_data.R`: Loads parameters and computes derived cost
    variables for analysis, outputting `param_vals.csv` for reference.
-   `scripts/costs_with_BUGS.R`: Runs probabilistic sensitivity analysis
    and total costing using BUGS output.
-   `scripts/posterior_predictive_analysis.R`: Runs posterior predictive
    simulations and compares expected-value and posterior predictive
    approaches.
-   `inst/extdata/parameters.csv`: Central parameter table used by
    `load_parameters()`.
-   `input_data/`: Cleaned and processed input datasets and BUGS output
    files.

## Requirements

-   R ≥ 4.0
-   Core dependencies: (list required packages)

## Citation

If you use **modularce** in your research, please cite:

``` bibtex
@software{modularce,
  title = {modularce: A Unified Framework for Multiple Component Cost-Effectiveness Models},
  author = {Green, N8than},
  year = {2024},
  url = {https://github.com/n8thangreen/modularce}
}
```

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md)
for guidelines.

## License

MIT License (see LICENSE.md)

## Support

For issues, questions, or suggestions, please open an
[issue](https://github.com/n8thangreen/modularce/issues) on GitHub.

## Current status:
This repo is currently under construction. 

------------------------------------------------------------------------

**Last updated**: September 2026
