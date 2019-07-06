
<!-- README.md is generated from README.Rmd. Please edit that file -->

# workbch

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/djnavarro/workbch.svg?branch=master)](https://travis-ci.org/djnavarro/workbch)
[![CRAN
status](https://www.r-pkg.org/badges/version/workbch)](https://cran.r-project.org/package=workbch)
<!-- badges: end -->

The goal of workbch is to provide tools for project management within R.
The design is very minimal at the moment. It allows the user to store
and edit basic metadata associated with projects. It allows you to
search, filter and navigate between projects. Prioritisation and
deadlines are supported, but financial information and time spent on a
project are not tracked.

## Installation

The workbch package has not been released on CRAN. However you can
install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("djnavarro/workbch")
```

The package is built around three families of functions:

  - the `job_*` functions create, delete and edit a job
  - the `view_*` functions display information about jobs
  - the `goto_*` functions navigate to projects and webpages

(though there are a few functions that don’t currently fit this scheme)

## Example 1: Getting started

The workbch package stores information about projects in a few files
stored within a directory referred to as the “workbch home”. The easiest
way to set this location in a persistent way is to edit the .Rprofile
file to include the following line:

``` r
options(workbch.home = "PATH_TO_PROJECT_FOLDER")
```

This ensures that whenever the workbch package is loaded it knows where
to find your project information. Once this is done, you can start
adding *jobs*. A “job” is intended to be much the same as a “project”,
but to avoid confusion with “RStudio projects” a different term is used.
Here’s how to add and view the jobs you have stored:

``` r
library(workbch)

view_joblist()
#> # A tibble: 0 x 0

job_create(
  name = "workitout", 
  description = "sip martinis and party in France", 
  owner = "britney"
)
#> Warning: 'britney' is not a known nickname

view_joblist()
#> # A tibble: 1 x 6
#>   name      owner   priority status deadline description                   
#>   <chr>     <chr>      <int> <chr>  <lgl>    <chr>                         
#> 1 workitout britney        1 active NA       sip martinis and party in Fra…
```

Projects can be deleted by name:

``` r
job_delete("workitout")
view_joblist()
#> # A tibble: 0 x 0
```

## Example 2: People, owners and teams

The workbch package stores a data base of names and nicknames, so you
can specify a person using their nickname instead of needing to type the
full name:

``` r
people_add("Beyoncé Knowles", "beyonce")
people_add("Kelly Rowland", "kelly")
people_add("Michelle Williams", "michelle")
people_list()
#> # A tibble: 3 x 2
#>   name              nickname
#>   <chr>             <chr>   
#> 1 Beyoncé Knowles   beyonce 
#> 2 Kelly Rowland     kelly   
#> 3 Michelle Williams michelle
```

Jobs can consist of multiple people on a *team* but the job must have a
single *owner*, a named team member who is responsible for the project.
When used in conjunction with nicknames, the `job_create()` function
allows you to specify the team efficiently:

``` r
job_create(
  name = "survivor",
  description = "Run a survival analysis",
  owner = "beyonce",
  team = c("kelly", "michelle"),
  priority = 1,
  status = "inactive"
)
```

The owner of a job will automatically be added to the team. As before we
can use `view_joblist()` to provide a summary of all listed jobs, which
in this case is only a single job, but you can also use `view_job()` to
look at a single job in more detail:

``` r
view_joblist()
#> # A tibble: 1 x 6
#>   name     owner           priority status  deadline description           
#>   <chr>    <chr>              <int> <chr>   <lgl>    <chr>                 
#> 1 survivor Beyoncé Knowles        1 inacti… NA       Run a survival analys…


view_job("survivor")
#> 
#> survivor : Run a survival analysis 
#> 
#>   owner    : Beyoncé Knowles 
#>   team     : Beyoncé Knowles, Kelly Rowland, Michelle Williams 
#>   priority : 1 
#>   status   : inactive 
#>   deadline : none 
#> 
#>   path = NA 
#>   0 notes
#>   0 tasks
```

## Example 3: Editing jobs

Internally, a “job” is represented as a list with the following fields

  - `name`: name of the project
  - `description`: brief description of the project
  - `owner`: should be a name or a nickname
  - `status`: should be “active”, “inactive”, “complete”, “abandoned”
  - `team`: should be a vector of names/nicknames (owner is
    automatically included)
  - `priority`: numeric
  - `deadline`: a date
  - `path`: path to the project home directory
  - `urls`: list of urls
  - `tasks`: list of tasks (currently doesn’t work)
  - `notes`: list of notes (currently doesn’t work)

When we added the “survival” job earlier, we specified some of these
fields but not others. There are three functions that you can use to
modify the properties of a job:

  - `job_edit()` can reset the value of any field
  - `job_edit_team()` makes it easier to edit the team
  - `job_edit_urls()` makes it easier to edit the list of webpages
    associated with a job

To be continued…

## On the name

The name `workbch` is an abbreviation of "work b\*\*ch". Depending on
your personal preferences you may wish to pronounce it in one of two
different ways:

  - As a truncation of “work bench”, because it’s supposed to be a
    metaphorical work bench,
  - As a reference to the pop song "work b\*\*ch", if you’re a Britney
    Spears fan.
