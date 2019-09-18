
# reset home to a tempory directory
loc <- tempdir()
options(workbch.home = loc)

test_that("pull_jobnames / pull_jobpaths work", {

  # delete the job file if for some reason it exists
  if(file.exists(job_file())) {
    file.remove(job_file())
  }

  # check that job_read returns NULL for an empty file
  jobs <- job_read()
  expect_null(jobs)

  # pull from empty jobnames and paths
  expect_equal(pull_jobnames(jobs), character(0))
  expect_equal(pull_jobpaths(jobs), character(0))

  # create a list of jobs directly from the constructor
  jobs <- suppressWarnings(list(toxic = new_job(
    jobname = "toxic", description = "a song", owner = "britney"
  )))
  job_write(jobs)

  # check job names and paths: named character vectors
  expect_equal(pull_jobnames(jobs), c(toxic = "toxic"))
  expect_equal(pull_jobpaths(jobs), c(toxic = NA_character_))

  # add a second job that does have a path
  jobs[["hitmebaby"]] <- suppressWarnings(new_job(
    jobname = "hitmebaby",
    description = "another song",
    owner = "britney",
    path = loc
  ))
  job_write(jobs)

  # check job names and paths: named character vectors
  expect_equal(pull_jobnames(jobs), c(toxic = "toxic", hitmebaby = "hitmebaby"))
  expect_equal(pull_jobpaths(jobs), c(toxic = NA_character_, hitmebaby = loc))

})

