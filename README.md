
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
#> 
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
  - `tasks`: list of tasks (NOT YET IMPLEMENTED)
  - `notes`: list of notes

When we added the “survival” job earlier, we specified some of these
fields but not others. There are three functions that you can use to
modify the properties of a job:

  - `job_edit()` can reset the value of any field
  - `job_edit_team()` makes it easier to edit the team
  - `job_edit_urls()` makes it easier to edit the list of webpages
    associated with a job

To illustrate, suppose we create a new job, called “toxic”:

``` r
people_add("Britney Spears", "britney")
people_add("Danielle Navarro", "danielle")

job_create(
  name = "toxic",
  description = "Estimate the LD50 dose",
  owner = "britney",
  priority = 2,
  status = "active",
  path = "~/projects/toxic",
  urls = list(
    github = "https://github.com/djnavarro/toxic",
    genius = "https://genius.com/Britney-spears-toxic-lyrics"
  )
)

view_joblist()
#> # A tibble: 2 x 6
#>   name     owner           priority status  deadline description           
#>   <chr>    <chr>              <int> <chr>   <lgl>    <chr>                 
#> 1 survivor Beyoncé Knowles        1 inacti… NA       Run a survival analys…
#> 2 toxic    Britney Spears         2 active  NA       Estimate the LD50 dose

view_job("toxic")
#> 
#> toxic : Estimate the LD50 dose 
#> 
#>   owner    : Britney Spears 
#>   team     : Britney Spears 
#>   priority : 2 
#>   status   : active 
#>   deadline : none 
#> 
#>   path = ~/projects/toxic 
#>   github = https://github.com/djnavarro/toxic 
#>   genius = https://genius.com/Britney-spears-toxic-lyrics 
#> 
#>   0 notes
#>   0 tasks
```

If at this point we realise that “Danielle” should have been listed on
the team (yeah, right) and the priority should have been set at 1:

``` r
job_edit_team("toxic", add = "danielle")
job_edit("toxic", priority = 1)
view_job("toxic")
#> 
#> toxic : Estimate the LD50 dose 
#> 
#>   owner    : Britney Spears 
#>   team     : Britney Spears, Danielle Navarro 
#>   priority : 1 
#>   status   : active 
#>   deadline : none 
#> 
#>   path = ~/projects/toxic 
#>   github = https://github.com/djnavarro/toxic 
#>   genius = https://genius.com/Britney-spears-toxic-lyrics 
#> 
#>   0 notes
#>   0 tasks
```

## Example 4: Filtering and prioritising

After a while one can easily end up with a lot of projects, and it can
be hard to find what you’re looking for (or, if you’re like me, get
anxious at seeing so many things on the to-do list). For example:

``` r
view_joblist()
#> # A tibble: 5 x 6
#>   name        owner         priority status  deadline description          
#>   <chr>       <chr>            <int> <chr>   <lgl>    <chr>                
#> 1 toxic       Britney Spea…        1 active  NA       Estimate the LD50 do…
#> 2 spinspinsu… Sneaker Pimps        1 active  NA       Check for periodicit…
#> 3 survivor    Beyoncé Know…        1 inacti… NA       Run a survival analy…
#> 4 hitmebaby   Britney Spea…        2 active  NA       Signal detection mod…
#> 5 boys        Lizzo                2 active  NA       Distributional assum…
```

A simple way to only see the high priority projects:

``` r
view_priorities()
#> # A tibble: 3 x 6
#>   name         owner         priority status  deadline description         
#>   <chr>        <chr>            <int> <chr>   <lgl>    <chr>               
#> 1 toxic        Britney Spea…        1 active  NA       Estimate the LD50 d…
#> 2 spinspinsug… Sneaker Pimps        1 active  NA       Check for periodici…
#> 3 survivor     Beyoncé Know…        1 inacti… NA       Run a survival anal…
```

More generally, `view_joblist()` and `view_priorities()` both allow you
to pass filtering expressions to `dplyr::filter()` to extract the subset
you’re interested in. Suppose I only want to see the high priority
active projects:

``` r
view_joblist(priority == 1 & status == "active")
#> # A tibble: 2 x 6
#>   name         owner         priority status deadline description          
#>   <chr>        <chr>            <int> <chr>  <lgl>    <chr>                
#> 1 toxic        Britney Spea…        1 active NA       Estimate the LD50 do…
#> 2 spinspinsug… Sneaker Pimps        1 active NA       Check for periodicit…
```

Indeed `view_priorities()` function is essentially a helper function to
avoid having to type `view_joblist(priority == 1)` on a regular basis.

## Example 5: Navigation

To open a webpage associated with a project, it is as simple as using
the `goto_url()` function:

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

## Example 6: Adding, viewing and deleting notes

Often it is handy to add small annotations to a job. The intent here is
not to use this as a substitute for proper documentation (that should
happen within the project itself) but as a quick and dirty “notes to
self” tool. You can add jobs using `job_add_note()`:

``` r
job_add_note("toxic", "check if this worked")
job_add_note("toxic", "i wonder if i should circulate this later")
```

You can view the notes linked to a job with `view_notes()`:

``` r
view_notes("toxic")
#> 
#> 2:  i wonder if i should circulate this later
#> 1:  check if this worked
```

The output is shown in chronological order (recent at the top). You can
remove a note by referring to its number:

``` r
job_delete_note("toxic", 1)
view_notes("toxic")
#> 
#> 2:  i wonder if i should circulate this later
```

As present the notes themselves do not show up directly when calling
`view_job()`. Instead there is a counter that indicates that a job has
notes associated with it:

``` r
view_job("toxic")
#> 
#> toxic : Estimate the LD50 dose 
#> 
#>   owner    : Britney Spears 
#>   team     : Britney Spears, Danielle Navarro 
#>   priority : 1 
#>   status   : active 
#>   deadline : none 
#> 
#>   path = ~/projects/toxic 
#>   github = https://github.com/djnavarro/toxic 
#>   genius = https://genius.com/Britney-spears-toxic-lyrics 
#> 
#>   1 notes
#>   0 tasks
```
