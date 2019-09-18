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

# create a job
job_create(
  jobname = "hitmebaby",
  description = "a song",
  owner = "britney",
  status = "masked",
  priority = 2,
  path = jobdir,
  tags = "awesome"
)
jb <- job_read()[[1]]

test_that("job_glimpse works", {

  # check that job_glimpse returns the job
  jb2 <- job_glimpse("hitmebaby")
  expect_equal(jb, jb2)

  # check that job_glimpse throws errors
  expect_error(job_glimpse(), "Could not detect current")
  expect_error(job_glimpse(1), "character and length 1")
  expect_error(job_glimpse("toxic"), "No job exists with")

  # check it defaults to current job if one exists
  wd <- setwd(jobdir)
  jb3 <- job_glimpse()
  expect_equal(jb, jb3)
  setwd(wd)

  # add a url and check
  job_modify("hitmebaby", url = "github | nopenope")
  jb4 <- job_glimpse("hitmebaby")
  expect_equal(job_read()[["hitmebaby"]], jb4)


})
