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

### Report Export (Optional)

#### HTML Export
Requires only `rmarkdown`:

```r
install.packages("rmarkdown")
install.packages("knitr")
install.packages("kableExtra")  # Optional, for better table styling
```

HTML reports are self-contained and can be opened offline in any web browser with all plots and styles embedded.

#### PDF Export
Additionally requires `tinytex`:

```r
install.packages("tinytex")

# Install TinyTeX for PDF rendering (one-time setup)
tinytex::install_tinytex()
```

**Note:** The app will work without these packages. Export buttons will show clear error messages with installation instructions if dependencies are missing.

## Running the App

```r
shiny::runApp()
```

Or from the command line:

```bash
Rscript -e "shiny::runApp()"
```

### Important Note for RStudio Users

**For reliable downloads:** Use the "Open in Browser" button (external browser) instead of the RStudio Viewer pane. The RStudio Viewer can sometimes block or interfere with file downloads.

To open in external browser:
1. Click the "Show in new window" icon in the Viewer pane, OR
2. Run: `shiny::runApp(launch.browser = TRUE)`

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
