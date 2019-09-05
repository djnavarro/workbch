
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

The package is built from four families of functions:

  - the `make_*` functions make jobs
  - the `set_*` functions edit jobs
  - the `view_*` functions display information about jobs
  - the `goto_*` functions navigate to projects and webpages
  - the `delete_*` functions delete jobs, tasks, etc

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
“project” in everyday life (i.e., a self-contained body of work of
some kind), but to avoid confusion with “RStudio projects” I’ve used a
different term. Here’s how to add and view the jobs you have stored:

``` r
library(workbch)

view_jobs()
#> # A tibble: 0 x 0

make_job(
  jobname = "workitout", 
  description = "sip martinis and party in France", 
  owner = "britney"
)
#> Warning: 'britney' is not a known nick name

view_jobs()
#> # A tibble: 1 x 5
#>   jobname   owner   priority status description                     
#>   <chr>     <chr>      <int> <chr>  <chr>                           
#> 1 workitout britney        1 active sip martinis and party in France
```

Jobs can be deleted by name:

``` r
delete_job("workitout")
view_jobs()
#> # A tibble: 0 x 0
```

## Example 2: People, owners and teams

The workbch package stores a data base of names and nicknames, so you
can specify a person using their nickname instead of needing to type the
full name:

``` r
set_person("Beyoncé Knowles", "beyonce")
#> added 'Beyoncé Knowles' with nickname 'beyonce'

set_person("Kelly Rowland", "kelly")
#> added 'Kelly Rowland' with nickname 'kelly'

set_person("Michelle Williams", "michelle")
#> added 'Michelle Williams' with nickname 'michelle'

workbch_people()
#> # A tibble: 3 x 2
#>   fullname          nickname
#>   <chr>             <chr>   
#> 1 Beyoncé Knowles   beyonce 
#> 2 Kelly Rowland     kelly   
#> 3 Michelle Williams michelle
```

Jobs can consist of multiple people on a *team* but the job must have a
single *owner*, a named team member who is responsible for that job.
When used in conjunction with nicknames, the `set_job()` function allows
you to specify the team efficiently:

``` r
make_job(
  jobname = "survivor",
  description = "Run a survival analysis",
  owner = "beyonce",
  team = c("kelly", "michelle"),
  priority = 1,
  status = "inactive"
)
```

The owner of a job will automatically be added to the team. As before we
can use `view_jobs()` to provide a summary of all listed jobs, which in
this case is only a single job, but you can also use `view_job()` to
look at a single job in more detail:

``` r
view_jobs()
#> # A tibble: 1 x 5
#>   jobname  owner           priority status   description            
#>   <chr>    <chr>              <int> <chr>    <chr>                  
#> 1 survivor Beyoncé Knowles        1 inactive Run a survival analysis

view_job("survivor")
#> 
#> survivor : Run a survival analysis 
#> 
#>   owner    : Beyoncé Knowles 
#>   team     : Beyoncé Knowles, Kelly Rowland, Michelle Williams 
#>   priority : 1 
#>   status   : inactive 
#>   tags     :
```

## Example 3: Editing jobs

Internally, a job is represented as a list with the following fields

  - `jobname`: name of the project
  - `description`: brief description of the project
  - `owner`: should be a name or a nickname
  - `status`: should be “active”, “inactive”, “complete”, “abandoned”
  - `team`: should be a vector of names/nicknames (owner is
    automatically included)
  - `priority`: numeric
  - `path`: path to the project home directory
  - `urls`: a tibble specifying urls linked to the job

When we added the “survival” job earlier, we specified some of these
fields but not others.

To illustrate, suppose we make a new job, called “toxic”:

``` r
set_person("Britney Spears", "britney")
#> added 'Britney Spears' with nickname 'britney'
set_person("Danielle Navarro", "danielle")
#> added 'Danielle Navarro' with nickname 'danielle'

make_job(
  jobname = "toxic",
  description = "Estimate the LD50 dose",
  owner = "britney",
  priority = 2,
  status = "active",
  path = "~/projects/toxic"
)

view_jobs()
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 2 x 5
#>   jobname  owner           priority status   description            
#>   <chr>    <chr>              <int> <chr>    <chr>                  
#> 1 survivor Beyoncé Knowles        1 inactive Run a survival analysis
#> 2 toxic    Britney Spears         2 active   Estimate the LD50 dose

view_job("toxic")
#> 
#> toxic : Estimate the LD50 dose 
#> 
#>   owner    : Britney Spears 
#>   team     : Britney Spears 
#>   priority : 2 
#>   status   : active 
#>   tags     :  
#> 
#>   locations: 
#>      [path] ~/projects/toxic
```

If at this point we realise that “Danielle” should have been listed on
the team for toxic (yeah, right) and the priority should have been set
at 1, we can edit the job. Similarly, if we want to add some URLS:

``` r
workbch_setjob(jobname = "toxic", priority = 1)
workbch_setjob(jobname = "toxic", add_team = "danielle")
workbch_setjob(jobname = "toxic", site = "github", link = "https://github.com/djnavarro/toxic")
workbch_setjob(jobname = "toxic", site = "genius", link = "https://genius.com/Britney-spears-toxic-lyrics")

view_job("toxic")
#> 
#> toxic : Estimate the LD50 dose 
#> 
#>   owner    : Britney Spears 
#>   team     : Britney Spears, Danielle Navarro 
#>   priority : 1 
#>   status   : active 
#>   tags     :  
#> 
#>   locations: 
#>      [path] ~/projects/toxic
#>      [genius] https://genius.com/Britney-spears-toxic-lyrics
#>      [github] https://github.com/djnavarro/toxic
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
#>   jobname       owner           priority status   description              
#>   <chr>         <chr>              <int> <chr>    <chr>                    
#> 1 toxic         Britney Spears         1 active   Estimate the LD50 dose   
#> 2 spinspinsugar Sneaker Pimps          1 active   Check for periodicities  
#> 3 survivor      Beyoncé Knowles        1 inactive Run a survival analysis  
#> 4 hitmebaby     Britney Spears         2 active   Signal detection modelli…
#> 5 boys          Lizzo                  2 active   Distributional assumptio…
```

A simple way to only see the high priority jobs:

``` r
view_priorities()
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 3 x 5
#>   jobname       owner           priority status   description            
#>   <chr>         <chr>              <int> <chr>    <chr>                  
#> 1 toxic         Britney Spears         1 active   Estimate the LD50 dose 
#> 2 spinspinsugar Sneaker Pimps          1 active   Check for periodicities
#> 3 survivor      Beyoncé Knowles        1 inactive Run a survival analysis
```

More generally, `view_jobs()` and `view_priorities()` both allow you to
pass filtering expressions to `dplyr::filter()` to extract the subset
you’re interested in. Suppose I only want to see the high priority
active jobs:

``` r
view_jobs(priority == 1 & status == "active")
#> Warning: Some job folders have moved or been deleted. Run
#> workbch_findjobs() to fix
#> # A tibble: 2 x 5
#>   jobname       owner          priority status description            
#>   <chr>         <chr>             <int> <chr>  <chr>                  
#> 1 toxic         Britney Spears        1 active Estimate the LD50 dose 
#> 2 spinspinsugar Sneaker Pimps         1 active Check for periodicities
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
