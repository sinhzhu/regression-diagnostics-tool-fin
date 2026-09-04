# Regression Diagnostics Tool

Tool for performing linear regressions on uploaded CSV.

> **SKIP TO BOTTOM FOR HOW TO RUN**


**Regression Diagnostics Tool Doc**

This document explains the R packages that this tool uses and how the calculations are performed inside the Shiny app. It is for those who want to understand what the tool does, and for me to understand as I'm developing it.

**Supported Data**

- This tool was primarily made and tested with CSV files from [data.census.gov](https://data.census.gov), specifically mostly American Community Surveys. These exports have artifacts that are not just plain numbers, such as dollars and percent signs and commas indicating thousands, that the app automatically cleans during upload.
- Specifically it cleans:
  - `$`, commas, percent signs, and `+/-` from values
  - Converts annotation codes (`*****`, `(X)`, `(D)`, `(N)`, `(S)`, `-`/`+`) to missing (`NA`)
  - Non-numeric columns (`Label (Grouping)`) and pure annotation columns

## Package Dependencies

The tool uses five R packages:

- shiny
  - Framework that powers the web app. The demo is run through shinyapps.io. Turns the script into a website that can run through a browser.
- ggplot2
  - R's library for creation of charts and graphs.
  - All plots used are drawn with ggplot2.
- car (Companion to Applied Regression)
  - Helper functions for regression analysis.
  - The VIF (Variance Inflation Factor) function in particular checks for multicollinearity, which is when two or more independent variables have such high correlation that it is difficult to differentiate which one is responsible for the outcome.
- lmtest
  - For linear model tests.
  - Used `bptest()` and `coeftest()`.
  - `bptest` is the Breusch-Pagan test for heteroskedasticity.
  - `coeftest` is used to rebuild the coefficient table when robust standard errors are requested.
- sandwich
  - Provides methods for computing better robust standard errors when the usual assumptions of regression are violated.
  - When the user ticks "Use robust (HC1) standard errors", sandwich provides the `vcovHC()` function, which computes a corrected covariance matrix. When heteroskedasticity exists in the data, this makes the standard errors and p-values provided in the coefficient table more reliable.


## HOW TO RUN

On the R console, paste:

```r
shiny::runApp("/path/to/Regression-Diagnostics-Tool/app.R")
```

The app will auto install any missing R packages on first run.
