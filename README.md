
<!-- README.md is generated from README.Rmd. Please edit that file -->

# projectr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/djnavarro/projectr.svg?branch=master)](https://travis-ci.org/djnavarro/projectr)
<!-- badges: end -->

The goal of projectr is to provide tools for project management within
R. The design is very minimal at the moment. It allows the user to store
and edit basic metadata associated with projects. It allows one to
search, filter and navigate between projects. Prioritisation and
deadlines are supported, but financial information and time spent on a
project are not tracked.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("djnavarro/projectr")
```

## Example 1

``` r
library(projectr)

# set the folder to store data (normally you'd specify this in
# the .Rprofile to ensure it's always available)
projectr_home(path = tempdir())
#> [1] "/tmp/RtmpPoGAAU"

# initially we have no jobs
view_joblist()
#> # A tibble: 0 x 0

# initially we know no people
people_list()
#> list()

# add some people
people_add(name = "Bora Horza Gobuchul", nickname = "horza")
people_add(name = "Perosteck Balveda", nickname = "balveda")
people_list()
#> # A tibble: 2 x 2
#>   name                nickname
#>   <chr>               <chr>   
#> 1 Bora Horza Gobuchul horza   
#> 2 Perosteck Balveda   balveda

# add a job with the required options only
job_create(name = "findmind", 
           description = "Find the refugee Culture Mind", 
           owner = "horza")

job_create(name = "getback", 
           description = "Return to Schaar to find her", 
           owner = "horza")

# now take a look
view_joblist()
#> # A tibble: 2 x 6
#>   name     owner           priority status deadline description            
#>   <chr>    <chr>              <int> <chr>  <lgl>    <chr>                  
#> 1 findmind Bora Horza Gob…        1 active NA       Find the refugee Cultu…
#> 2 getback  Bora Horza Gob…        1 active NA       Return to Schaar to fi…

# you can look at a specific job:
view_job("findmind")
#> 
#> findmind : Find the refugee Culture Mind 
#> 
#>   owner    : Bora Horza Gobuchul 
#>   team     : Bora Horza Gobuchul 
#>   priority : 1 
#>   status   : active 
#>   deadline : none 
#> 
#>   path = NA 
#>   0 notes
#>   0 tasks
```

## Example 2

Jobs can be specified in more detail

``` r
job_create(name = "findhorza", 
           description = "Find Horza", 
           owner = "balveda",
           status = "active",
           priority = 1,
           deadline = NA,
           path = "~/myprojects/findhorza",
           urls = list(
             github = "https://github.com/balveda/notarealthing",
             overleaf = "https://overleaf.com/dklgjslkfjaslkfj")
           )

# now take a look
view_joblist()
#> # A tibble: 3 x 6
#>   name     owner           priority status deadline description            
#>   <chr>    <chr>              <int> <chr>  <lgl>    <chr>                  
#> 1 findmind Bora Horza Gob…        1 active NA       Find the refugee Cultu…
#> 2 getback  Bora Horza Gob…        1 active NA       Return to Schaar to fi…
#> 3 findhor… Perosteck Balv…        1 active NA       Find Horza

# filter the job list (currently doesn't support nicknames)
view_joblist(owner == "Bora Horza Gobuchul")
#> # A tibble: 2 x 6
#>   name     owner           priority status deadline description            
#>   <chr>    <chr>              <int> <chr>  <lgl>    <chr>                  
#> 1 findmind Bora Horza Gob…        1 active NA       Find the refugee Cultu…
#> 2 getback  Bora Horza Gob…        1 active NA       Return to Schaar to fi…
```

Everything is stored in plain text files. There is a JSON file
containing the jobs, and a CSV file containing the table of names. To
avoid the tediousness of calling `projectr_home` every session you can
include a line like this in .Rprofile

    options(projectr.home = "~/GitHub/utilities/projects/")

A “job” is stored as a list that contains the following fields

  - `name`: name of the project to create
  - `description`: brief description of the project
  - `owner`: should be a name or a nickname
  - `status`: should be “active”, “inactive”, “complete”, “abandoned”
  - `team`: should be a vector of names/nicknames (owner is
    automatically included)
  - `priority`: numeric
  - `deadline`: a date
  - `path`: path to the project home directory
  - `urls`: list of urls
  - `tasks`: list of tasks
  - `notes`: list of notes

The intention is to allow jobs to include a set of “tasks” which are
structured objects in their own right, and to allow jobs and tasks to be
filtered by the due date of tasks (e.g., you should be able to pull out
of a list of tasks due before a specific date). The functionality for
“notes” hasn’t been determined yet, but the intention is *not* to turn
this into a project log or anything like that. Each project should
maintain its own logs, readmes, etc: that is deliberately outside the
scope of projectr\!

Functions are organised as:

  - `job_*` functions create, delete and edit job
  - `view_*` functions display information about jobs
  - `goto_*` functions navigate to projects/url

There are a few functions that don’t currently fit this scheme. WIP.
