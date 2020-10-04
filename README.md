
<!-- README.md is generated from README.Rmd. Please edit that file -->

# data.handling

<!-- badges: start -->

<!-- badges: end -->

The data.handling package includes several tutorials to reach
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

You can install `data.handling` from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Bio302-UiB/data-handling")
```

To access the tutorials, you first need to load the package.

The tutorials should appear in the tutorials tab. This is normally in
the top right pane of Rstudio next to the Environment and History tabs.

You can also get a list of the available tutorials by running

``` r
learnr::available_tutorials(package = "data.handling")
#> Available tutorials:
#> * data.handling
#>   - Dates-and-times   : "Working with dates and times"
#>   - text-manipulation : "Processing text with `stringr`"
#>   - using-dplyr       : "Using dplyr"
```

Tutorials can be run with

``` r
learnr::run_tutorial("Dates-and-times", package = "data.handling")
```
