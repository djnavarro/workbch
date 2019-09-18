
# set up
loc <- tempdir()
options(workbch.home = loc)

# delete the job file if for some reason it exists
if(file.exists(job_file())) {
  file.remove(job_file())
}

# create path
jobdir <- file.path(loc, "hitmebaby")
jobdir2 <- file.path(loc, "slave4u")
if(!dir.exists(jobdir)) dir.create(jobdir)
sentinel <- file.path(jobdir, ".workbch")

test_that("job_create works", {

  # create the job manually, no sentinel file, write nothing
  jobs <- list()
  jobs[["hitmebaby"]] <- new_job(
    jobname = "hitmebaby",
    description = "a song",
    owner = "britney",
    path = jobdir,
    sentinel = FALSE
  )

  expect_false(file.exists(sentinel))
  expect_false(file.exists(job_file()))

  # create the job
  job_create(
    jobname = "hitmebaby",
    description = "a song",
    owner = "britney",
    path = jobdir
  )

  expect_true(file.exists(sentinel))
  expect_true(file.exists(job_file()))

  # read the job file
  jobs2 <- job_read()
  jobs[["hitmebaby"]]$idstring <- jobs2[["hitmebaby"]]$idstring

  # expect equal in every respect except idstring
  expect_equal(jobs, jobs2)

  # expect errors when given insufficent input
  expect_error(job_create())
  expect_error(job_create("hitmebaby"))
  expect_error(job_create("toxic"))
  expect_error(job_create("toxic", "a song"))

  # expect success when given minimal input and maximal input
  expect_silent(job_create("toxic", "a song", "britney"))
  expect_silent(
    job_create(
      jobname = "slave4u",
      description = "another song",
      owner = "britney",
      status = "active",
      priority = 3,
      tags = "aaa | bbb",
      path = jobdir2
    )
  )

  # expect that newly created jobs are appended
  jobs3 <- job_read()
  expect_length(jobs3, 3)
  expect_named(jobs3, c("hitmebaby", "toxic", "slave4u"))

})
