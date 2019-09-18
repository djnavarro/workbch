# set up
loc <- tempdir()
options(workbch.home = loc)

# delete the job file if for some reason it exists
if(file.exists(job_file())) {
  file.remove(job_file())
}

# create path
jobdir <- file.path(loc, "hitmebaby")
if(!dir.exists(jobdir)) dir.create(jobdir)

# create jobs, one with a path
job_create(
  jobname = "hitmebaby",
  description = "a song",
  owner = "britney",
  path = jobdir
)
job_create(
  jobname = "toxic",
  description = "a song",
  owner = "britney"
)

test_that("job_home works", {

  # the three scenarios when a string is given
  expect_equal(job_home("hitmebaby"), jobdir)
  expect_error(job_home("toxic"), "No path known for")
  expect_error(job_home("dfasdfsdgdf"), "No job exists with")

  # make sure we're hitting verify_onestring
  expect_error(job_home(1), "must be character and length 1")

  # make sure we're hitting job_getcurrent
  expect_error(job_home(), "Could not detect current job")
  wd <-setwd(jobdir)
  expect_equal(job_home(), jobdir)
  setwd(wd)

})
