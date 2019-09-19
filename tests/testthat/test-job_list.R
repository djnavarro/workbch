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

# create jobs
job_create(
  jobname = "hitmebaby",
  description = "a song",
  owner = "britney",
  status = "masked",
  priority = 2,
  path = jobdir,
  tags = "awesome"
)
job_create(
  jobname = "toxic",
  description = "another song",
  owner = "britney",
  status = "masked",
  priority = 1,
  tags = "awesome | fun"
)

test_that("job_list works", {

  # make sure we have a jobs file
  expect_length(job_read(), 2)

  expect_equal(nrow(job_list()), 2)
  expect_named(
    job_list(),
    c("jobname", "owner", "priority", "status", "description")
  )
  expect_equal(job_list()$jobname, c("toxic", "hitmebaby"))

})
