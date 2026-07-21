# modularce

> A Unified Framework for Multiple Component Cost-Effectiveness Models in R

## Overview

**modularce** provides a comprehensive, structured approach to building cost-effectiveness models that combine multiple modeling components—such as decision trees and Markov models. Rather than linking these models in an ad-hoc fashion, this framework promotes modular, robust, and reusable code design for health economic evaluations.

## Problem Statement

Health economic modelling and cost-effectiveness analysis (CEA) frequently requires integrating multiple model structures:
- **Decision trees** for initial pathways or early decisions
- **Markov models** for long-term state transitions
- Custom components for specialized analyses

However, these integrations are often implemented inconsistently, leading to:
- ❌ Less robust and error-prone code
- ❌ Poor reusability across projects
- ❌ Difficult maintenance and validation
- ❌ Reduced transparency and reproducibility

## Solution

This framework presents several approaches to linking multiple model components, ranging from simple to advanced implementations, with different complexity-generalisability trade-offs.

## Key Features

- 🔧 **Modular design** — Build components independently and combine them seamlessly
- 📊 **Multiple approaches** — Choose the complexity level that suits your needs
- 🏥 **Evidence-based** — Grounded in real-life case studies from published research
- 📚 **Well-documented** — Comprehensive tutorials and practical examples
- 🔁 **Reusable** — Write once, apply across multiple analyses

## Getting Started

### Installation

```r
# Install from GitHub using devtools
devtools::install_github("n8thangreen/modularce")
```

### Basic Example

```r
library(modularce)

# Create your model components
decision_tree <- create_decision_tree(...)
markov_model <- create_markov_model(...)

# Link them together
combined_model <- link_models(decision_tree, markov_model)

# Run analysis
results <- run_analysis(combined_model)
```

## Documentation

- [Introduction](docs/introduction.md) — Overview and conceptual framework
- [Tutorials](docs/tutorials/) — Step-by-step guides for different approaches
- [Case Studies](docs/case_studies/) — Real-world examples from published literature
- [API Reference](docs/reference.md) — Complete function documentation

## Use Cases

This framework is ideal for:
- Pharmaceutical and medical device cost-effectiveness analyses
- Public health interventions and disease prevention models
- Healthcare technology assessments
- Budget impact analyses
- Multi-state transition models with initial decision nodes

## Approaches Included

1. **Basic Integration** — Simple linking for straightforward models
2. **Intermediate Framework** — Enhanced structure with better parameter management
3. **Advanced Architecture** — Fully modular with extensibility for complex scenarios

## Requirements

- R ≥ 4.0
- Core dependencies: (list required packages)

## Citation

If you use **modularce** in your research, please cite:

```bibtex
@software{modularce,
  title = {modularce: A Unified Framework for Multiple Component Cost-Effectiveness Models},
  author = {Green, N8than},
  year = {2024},
  url = {https://github.com/n8thangreen/modularce}
}
```

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

[Specify License - e.g., MIT, GPL-3.0, etc.]

## Support

For issues, questions, or suggestions, please open an [issue](https://github.com/n8thangreen/modularce/issues) on GitHub.

## Related Resources

- [Health Economic Modeling in R](https://example.com)
- [Markov Models Tutorial](https://example.com)
- [Decision Tree Analysis Guide](https://example.com)

---

**Last updated**: July 2026
