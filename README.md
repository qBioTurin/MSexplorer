# MSexplorer

MSexplorer is an R/Shiny application for exploring microbiome analysis results from a multiple sclerosis study. It provides interactive views for differential abundance results, PCA, k-means clustering, 3D PCA, clinical association tests, survival analysis, and EDSS flow visualizations.

## Preview

<p align="center">
  <img src="www/home.png" alt="MSexplorer home graphic" width="320">
</p>

<p align="center">
  <img src="www/loghi.png" alt="Institutional and project logos" width="850">
</p>

## Repository contents

- `app.R`, `ui.R`, `server.R`: Shiny application entry point and interface/server code.
- `Functions.R`: plotting, clustering, statistical testing, and helper functions.
- `www/Data.Rds`, `www/metadata.Rds`: derived data objects used by the app.
- `www/home.png`, `www/loghi.png`: images used by the app and displayed in this README.
- `www/tables/001_bacteria_maaslin.tsv`: differential abundance results displayed in the data explorer.

## Run locally

Install the required R packages:

```r
install.packages(c(
  "DT", "shinybusy", "shiny", "shinydashboard", "shinyjs", "plotly",
  "ggplot2", "dplyr", "ggalluvial", "patchwork", "ggrepel",
  "ggfortify", "ggforce", "factoextra", "tidyr", "purrr", "ggmosaic",
  "clusterCrit", "concaveman", "survival", "survminer",
  "RColorBrewer", "viridisLite"
))
```

Start the application from the repository root:

```r
shiny::runApp(".")
```

## Citation

Please cite the associated publication and this repository if you use MSexplorer. Update `CITATION.cff` with the final manuscript DOI and full author list before public release.

## License

No software license has been selected yet. Add the license approved by the authors/institution before making the repository public or linking it from a publication.
