
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

# Example

This is a basic example.

## Create new D2SB project

The following copies the [template project
directory](https://github.com/petersonR/D2SBtools/tree/main/inst/templates/project/skeleton)
and thereby creates a new project.

``` r
# write a local verson to be able to read
create_project(
  project_name = "temporary_project", 
  path = NULL, # creates in current directory
  open = FALSE, # Opens in RStudio
  
  # Set parameters in Admin/project.yml (can be done later too)
  title = "My D2SB Project",
  analyst = "Me",
  status = "active",
  pi = "My PI",
  data_path_from_sharepoint = "Projects/template_project"
)
#> ✅ Created project 'temporary_project' at /Users/rpterson/Library/CloudStorage/OneDrive-UniversityofIowa/Software/D2SBtools/temporary_project
```

## Get all available project data

Moving into this temporary project, we can then run the following
commands

``` r
get_project_data()
#>                         project_name                         date_created 
#>                  "temporary_project"                         "2025-11-10" 
#>                                title                              analyst 
#>                    "My D2SB Project"                                 "Me" 
#>                               status                                   pi 
#>                             "active"                              "My PI" 
#>            data_path_from_sharepoint                                 dept 
#>          "Projects/template_project"           "<Department or Division>" 
#>                           start_date                         expected_end 
#>                       "<YYYY-MM-DD>"                       "<YYYY-MM-DD>" 
#>                    current_data_date        biostat_effort_planned.person 
#>                         "YYYY-MM-DD"                             "<name>" 
#>          biostat_effort_planned.role           biostat_effort_planned.fte 
#>                            "faculty"                               "0.05" 
#>         biostat_effort_planned.start           biostat_effort_planned.end 
#>                       "<YYYY-MM-DD>"                       "<YYYY-MM-DD>" 
#>        biostat_effort_planned.source biostat_effort_planned.admin_contact 
#>                              "grant"                            "Unknown"
```

## Get specific project data

``` r
get_project_data("title")
#>             title 
#> "My D2SB Project"
```

## Get sharepoint path

``` r
sp_path()
#> [1] "~/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofIowa/Data2Science BRIDGE - Documents//Projects/template_project/"
```

## List files on sharepoint

``` r
list.files(sp_path())
#> [1] "DataProcessed" "DataRaw"
```

## Read data off sharepoint

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

## Create new report

A D2SB template report is available via this package. A new report based
on this template can be created via:

``` r
create_report(path = "Reports/new.qmd")
```
