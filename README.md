
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

The workbch (“work bench”) package provides simple utility functions to
help the R user keep track of projects and navigate between them. The
package is designed around the concept of jobs, where a job might
correspond to an RStudio project, a git repository, a research project
or indeed all of the above. Jobs are assumed to be stored in a single
folder, but can be associated with URLs (e.g., on GitHub, Overleaf, OSF,
or elsewhere). The package is intended to be used interactively, though
most functions can be called from scripts if needed.

## Installation

The workbch package has not been released on CRAN. You can install the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("djnavarro/workbch")
```

## Overview

The package consists of nine functions. Three functions are used to
create, modify, and delete jobs

  - `job_create()`. Create a new job
  - `job_modify()`. Modify or delete an existing job
  - `job_seek()`. Scans a directory recursively to find jobs

There are three functions that are useful for navigation:

  - `job_open()`. Opens an RStudio project or changes working directory
  - `job_openurl()`. Opens a URL associated with a job in a browser
    window
  - `job_home()`. Returns the path to the job folder

There are three functions that are useful for keeping track of projects:

  - `job_gitreport()`. Shows the git status of all jobs
  - `job_glimpse()`. Shows all information stored about a job
  - `job_list()`. Displays a table summarising all jobs, or a subset of
    them.
