
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biostats.tutorials

<!-- badges: start -->

<!-- badges: end -->

The biostats.tutorials package includes several tutorials to reach
reproducible data analysis with R with the tidyverse package.

# Data handling with tidyverse

Importing and cleaning data usually take up the vast majority of time
spent working on a project. The statistical analysis is often fairly
quick and done in a few lines of code. To make an analysis reproducible,
we need to do all the data processing with code (rather than editing
files in Excel).

This project is designed to help you process your raw data entirely with
R so that it is ready for plotting and statistical analysis.

## Installation

You can install `biostats.tutorials` from
[GitHub](https://github.com/biostats-r/biostats.tutorials) with:

``` r
# install.packages("remotes")
remotes::install_github("biostats-r/biostats.tutorials")
```

To access the tutorials, you first need to load the package.

The tutorials should appear in the tutorials tab. This is normally in
the top right pane of Rstudio next to the Environment and History tabs.

You can also get a list of the available tutorials by running

``` r
learnr::available_tutorials(package = "biostats.tutorials")
#> Available tutorials:
#> * biostats.tutorials
#>   - Dates-and-times   : "Working with dates and times"
#>   - naming-objects    : "Naming objects"
#>   - text-manipulation : "Processing text with `stringr`"
#>   - tidying-data      : "Tidying data with tidyr"
#>   - using-dplyr       : "Using dplyr"
```

Tutorials can be run with

``` r
learnr::run_tutorial("Dates-and-times", package = "biostats.tutorials")
```
