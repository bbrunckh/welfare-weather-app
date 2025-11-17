
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{wiseapp}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{wiseapp}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
wiseapp::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-11-14 14:20:59 GMT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading wiseapp
#> ── R CMD check results ───────────────────────────────── wiseapp 0.0.0.9000 ────
#> Duration: 1.6s
#> 
#> ❯ checking package dependencies ... ERROR
#>   Namespace dependencies missing from DESCRIPTION Imports/Depends entries:
#>     'golem', 'shiny'
#>   
#>   See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
#>   manual.
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
