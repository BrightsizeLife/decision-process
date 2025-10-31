# Decision Space — Two-Choice, Five-Aspect Simulator

A Shiny application for comparing two choices across multiple decision aspects using Monte Carlo simulation.

## Features

- **Interactive Decision Space:** Configure up to 5 decision aspects with importance and presence sliders
- **Aspect Inclusion Control:** Toggle aspects on/off to see how they affect the analysis
- **Comprehensive Report:** View detailed statistics including probabilities, thresholds, and distributions
- **PDF Export:** Download a formatted report with all statistics and visualizations

## Requirements

### Core Dependencies
- `shiny`
- `bslib`
- `ggplot2`
- `dplyr`
- `tidyr`
- `purrr`
- `shinyjs`
- `scales`

### PDF Export (Optional)

To enable PDF report downloads, you'll need:

```r
install.packages("rmarkdown")
install.packages("knitr")
install.packages("kableExtra")
install.packages("tinytex")

# Install TinyTeX for PDF rendering
tinytex::install_tinytex()
```

**Note:** The app will work without these packages, but the PDF download button will show an error if rmarkdown is not available.

## Running the App

```r
shiny::runApp()
```

Or from the command line:

```bash
Rscript -e "shiny::runApp()"
```

## Report Statistics

The Report tab provides:

- **Mean scores** for each choice
- **Mean difference** (A - B) with 90% interval width
- **P(A > B)** and **P(B > A)**: Probability of one choice beating the other
- **P(within 5%)**: Probability that choices are within 5% of each other
- **Threshold probabilities**: Probability of one choice exceeding the other by various margins (5%, 10%, 20%, 50%)
- **Visualizations**: Density plots for score distributions and differences

## Methodology

Each decision aspect uses Beta distributions parameterized by:
- **Importance:** Relative weight (0 = low, 1 = high)
- **Uncertainty:** Confidence in importance (0 = very certain, 1 = very uncertain)
- **Presence (A & B):** Degree to which each choice exhibits this aspect
- **Uncertainty (A & B):** Confidence in presence

Overall scores are computed as: Score = Σ(Importance × Presence)

All probabilities are estimated from 5,000 Monte Carlo draws.
