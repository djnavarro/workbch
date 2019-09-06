
<!-- README.md is generated from README.Rmd. Please edit that file -->

# workbch

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/djnavarro/workbch.svg?branch=master)](https://travis-ci.org/djnavarro/workbch)
[![CRAN
status](https://www.r-pkg.org/badges/version/workbch)](https://cran.r-project.org/package=workbch)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/workbch/branch/master/graph/badge.svg)](https://codecov.io/gh/djnavarro/workbch?branch=master)
<!-- badges: end -->

The workbch package provides a “work bench” of tools for project
management within R, based around the concept of a “job” (which might
map to a single RStudio project or a single git repository). Jobs can be
linked to multiple URLs, etc. In addition to basic tracking, searching
and filtering, the package provides some tools to navigate between jobs,
browse relevant websites, and check the git status of repositories
linked to a job.

## Installation

The workbch package has not been released on CRAN. You can install the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("djnavarro/workbch")
```

## Example 1: Getting started

The workbch package stores information in a few files that are stored
within a directory referred to as the “workbch home”. The easiest way to
set this location in a persistent way is to edit the .Rprofile file to
include the following line:

``` r
options(workbch.home = "PATH_TO_FOLDER")
```

This ensures that when the workbch package is loaded it knows where to
find information about jobs. Once this is done, you can start adding
jobs\! A “job” is intended to have roughly the same meaning as a
“project” in everyday life (i.e., a self-contained body of work), but
to avoid confusion with “RStudio projects” I’ve used a different term.
Here’s how to add and view the jobs you have stored:

``` r
library(workbch)

view_jobs()
#> # A tibble: 0 x 0

job_create(
  jobname = "workitout", 
  description = "Sip martinis and party in France", 
  owner = "Britney Spears"
)

view_jobs()
#> # A tibble: 1 x 5
#>   jobname   owner          priority status description                     
#>   <chr>     <chr>             <int> <chr>  <chr>                           
#> 1 workitout Britney Spears        1 active Sip martinis and party in France
```

## Example 3: Editing jobs

To illustrate, suppose we make a new job, called “toxic”:

``` r
job_create(
  jobname = "toxic",
  description = "Estimate the LD50 dose",
  owner = "Britney Spears",
  priority = 2,
  status = "active",
  path = "~/projects/toxic"
)
view_jobs()
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 2 x 5
#>   jobname   owner          priority status description                     
#>   <chr>     <chr>             <int> <chr>  <chr>                           
#> 1 workitout Britney Spears        1 active Sip martinis and party in France
#> 2 toxic     Britney Spears        2 active Estimate the LD50 dose
```

If at this point we realise the priority should have been set at 1, or
we want to add some URLS:

``` r
work_modify(jobname = "toxic", priority = 1)
work_modify(jobname = "toxic", site = "github", link = "https://github.com/djnavarro/toxic")
work_modify(jobname = "toxic", site = "genius", link = "https://genius.com/Britney-spears-toxic-lyrics")
view_jobs()
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 2 x 5
#>   jobname   owner          priority status description                     
#>   <chr>     <chr>             <int> <chr>  <chr>                           
#> 1 toxic     Britney Spears        1 active Estimate the LD50 dose          
#> 2 workitout Britney Spears        1 active Sip martinis and party in France
```

If `jobname` argument is not specified, the workbch package attempts to
see if the use is working within a known job. It checks for this in two
ways. If the current RStudio project matches a known job, then that job
is used by default. Failing that, the working directory is checked.

## Example 4: Filtering and prioritising

After a while one can easily end up with a lot of jobs, and it can be
hard to find what you’re looking for (or, if you’re like me, get anxious
at seeing so many things that you have to do). For example:

``` r
view_jobs()
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 5 x 5
#>   jobname       owner         priority status description                  
#>   <chr>         <chr>            <int> <chr>  <chr>                        
#> 1 toxic         Britney Spea…        1 active Estimate the LD50 dose       
#> 2 workitout     Britney Spea…        1 active Sip martinis and party in Fr…
#> 3 spinspinsugar Sneakerpimps         1 active Check for periodicities      
#> 4 hitmebaby     Britney Spea…        2 active Signal detection modelling   
#> 5 boys          Lizzo                2 active Distributional assumptions
```

A simple way to only see the high priority jobs:

``` r
view_priorities()
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 3 x 5
#>   jobname       owner         priority status description                  
#>   <chr>         <chr>            <int> <chr>  <chr>                        
#> 1 toxic         Britney Spea…        1 active Estimate the LD50 dose       
#> 2 workitout     Britney Spea…        1 active Sip martinis and party in Fr…
#> 3 spinspinsugar Sneakerpimps         1 active Check for periodicities
```

More generally, `view_jobs()` and `view_priorities()` both allow you to
pass filtering expressions to `dplyr::filter()` to extract the subset
you’re interested in. Suppose I only want to see the high priority
active jobs:

``` r
view_jobs(priority == 1 & status == "active")
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 3 x 5
#>   jobname       owner         priority status description                  
#>   <chr>         <chr>            <int> <chr>  <chr>                        
#> 1 toxic         Britney Spea…        1 active Estimate the LD50 dose       
#> 2 workitout     Britney Spea…        1 active Sip martinis and party in Fr…
#> 3 spinspinsugar Sneakerpimps         1 active Check for periodicities
```

Indeed `view_priorities()` function is essentially a helper function to
avoid having to type `view_jobs(priority == 1)` on a regular basis.

## Example 5: Navigation

To open a webpage associated with a job, it is as simple as using the
`goto_url()` function:

``` r
goto_url("toxic", "github")
```

To open the corresponding RStudio project (assuming that there is an
RStudio project file located in the directory specified as the project
path)…

``` r
goto_project("toxic")
```

If there is no RStudio project at the relevant location, or the RStudio
API is not available (i.e., RStudio is not running), all this function
will do is use `setwd()` to change the working directory.
