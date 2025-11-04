
<!-- README.md is generated from README.Rmd. Please edit that file -->

# D2SBtools

<!-- badges: start -->

<!-- badges: end -->

The goal of D2SBtools is to provide helpful internal tools for D2SB
projects.

## Installation

You can install the development version of D2SBtools like so:

``` r
devtools::install_github("petersonR/D2SBtools")
```

Then load the library:

``` r
library(D2SBtools)
```

## Example

This is a basic example:

# Get all available project data

``` r
get_project_data()
#>                       title                     analyst 
#>           "My D2SB Project"                        "Me" 
#>                      status                          pi 
#>                    "active"                     "My PI" 
#>   data_path_from_sharepoint 
#> "Projects/template_project"
```

# Get specific project data

``` r
get_project_data("title")
#>             title 
#> "My D2SB Project"
```

# Get sharepoint path

``` r
sp_path()
#> [1] "~/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofIowa/Data2Science BRIDGE - Documents/Projects/template_project/"
```

# List files on sharepoint

``` r
list.files(sp_path())
#> [1] "DataProcessed" "DataRaw"
```

# Read data off sharepoint

``` r
test_data <- read_csv(sp_path("DataRaw/test_data.csv")) 
#> Rows: 13 Columns: 25
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (13): ht, raceth, smoking, drinkany, exercise, physact, globrat, medcond...
#> dbl (12): hdl1, age, weight, bmi, waist, whr, glucose, ldl, hdl, tg, sbp, dbp
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
test_data
#> # A tibble: 13 × 25
#>     hdl1 ht         age raceth smoking drinkany exercise physact globrat medcond
#>    <dbl> <chr>    <dbl> <chr>  <chr>   <chr>    <chr>    <chr>   <chr>   <chr>  
#>  1    48 placebo     70 Afric… no      no       no       much m… good    no     
#>  2    48 placebo     62 Afric… no      no       no       much l… good    yes    
#>  3    66 hormone…    69 White  no      no       no       about … good    no     
#>  4    57 placebo     64 White  yes     yes      no       much l… good    yes    
#>  5    35 placebo     65 White  no      no       no       somewh… good    no     
#>  6    53 hormone…    68 Afric… no      yes      no       about … good    no     
#>  7    54 hormone…    69 White  no      no       yes      much m… very g… no     
#>  8    57 hormone…    61 White  no      yes      yes      about … very g… yes    
#>  9    39 hormone…    62 White  yes     yes      no       somewh… good    no     
#> 10    57 placebo     72 Afric… no      no       no       about … fair    no     
#> 11    51 hormone…    73 White  no      no       no       somewh… good    yes    
#> 12    55 placebo     52 White  yes     no       no       somewh… good    yes    
#> 13    52 hormone…    57 White  yes     no       no       somewh… fair    no     
#> # ℹ 15 more variables: htnmeds <chr>, statins <chr>, diabetes <chr>,
#> #   dmpills <chr>, insulin <chr>, weight <dbl>, bmi <dbl>, waist <dbl>,
#> #   whr <dbl>, glucose <dbl>, ldl <dbl>, hdl <dbl>, tg <dbl>, sbp <dbl>,
#> #   dbp <dbl>
```
