
<!-- README.md is generated from README.Rmd. Please edit that file -->

# projectr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of projectr is to let me manage my projects within R. It’s very
minimal in design, and does not track hours spent.

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
#> [1] "/tmp/RtmpRSRnGS"

# initially we have no jobs
job_list()
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
job_list()
#> # A tibble: 2 x 6
#>   name     owner           priority status deadline description            
#>   <chr>    <chr>              <int> <chr>  <lgl>    <chr>                  
#> 1 findmind Bora Horza Gob…        1 active NA       Find the refugee Cultu…
#> 2 getback  Bora Horza Gob…        1 active NA       Return to Schaar to fi…
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
job_list()
#> # A tibble: 3 x 6
#>   name     owner           priority status deadline description            
#>   <chr>    <chr>              <int> <chr>  <lgl>    <chr>                  
#> 1 findmind Bora Horza Gob…        1 active NA       Find the refugee Cultu…
#> 2 getback  Bora Horza Gob…        1 active NA       Return to Schaar to fi…
#> 3 findhor… Perosteck Balv…        1 active NA       Find Horza

# filter the job list (currently doesn't support nicknames)
job_list(owner == "Bora Horza Gobuchul")
#> # A tibble: 2 x 6
#>   name     owner           priority status deadline description            
#>   <chr>    <chr>              <int> <chr>  <lgl>    <chr>                  
#> 1 findmind Bora Horza Gob…        1 active NA       Find the refugee Cultu…
#> 2 getback  Bora Horza Gob…        1 active NA       Return to Schaar to fi…
```

## Notes

Current functions implemented (and not particularly well-documented):

  - `job_browse_url`: opens a URL associated with a job
  - `job_create`: creates a new job
  - `job_delete`: deletes an existing job
  - `job_edit`: change a field within a job
  - `job_edit_urls`: convenience function to edit a urls within a job
  - `job_list`: list all jobs (allows filtering via dplyr::filter)
  - `job_open`: open the RStudio project located at the job path
  - `job_show`: display all information about a job (currently dumps a
    list\!)
  - `people_add`: add a named person with a nickname to the table
  - `people_list`: display the table of names
  - `projectr_home`: get or set the location where projectr stores files

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
  - `team`: should be a vector of names/nicknames
  - `priority`: numeric
  - `deadline`: a date
  - `path`: path to the project home directory
  - `urls`: list of urls
  - `notes`: list of notes
  - `tasks`: list of tasks

The intention is to allow jobs to include a set of “tasks” which are
structured objects in their own right, and to allow jobs and tasks to be
filtered by the due date of tasks (e.g., you should be able to pull out
of a list of tasks due before a specific date). The functionality for
“notes” hasn’t been determined yet, but the intention is *not* to turn
this into a project log or anything like that. Each project should
maintain its own logs, readmes, etc: that is deliberately outside the
scope of projectr\!
