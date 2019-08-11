
# reset home to a tempory directory
loc <- tempdir()
options(workbch.home = loc)

test_that("job_file returns the correct path", {
  expect_equal(job_file(), file.path(loc, "workbch_jobs.json"))
})

test_that("reading and writing jobs works", {

  # delete the job file if for some reason it exists
  if(file.exists(job_file())) {
    file.remove(job_file())
  }

  # check that job_read returns NULL for an empty file
  jobs <- job_read()
  expect_null(jobs)

  # while we're here, lets try to read empty jobnames and paths
  expect_equal(job_getnames(jobs), character(0))
  expect_equal(job_getpaths(jobs), character(0))

  # verification tests (here because that was the original file structure)
  expect_true(verify_jobmissing("toxic", jobs, strict = FALSE))
  expect_true(verify_jobmissing("hitmebaby", jobs, strict = FALSE))

  # create a list of jobs directly from the constructor
  jobs <- suppressWarnings(list(toxic = new_job(
    jobname = "toxic", description = "a song", owner = "britney"
  )))
  job_write(jobs)

  # check job read has the correct info
  expect_equal(job_read(), jobs)

  # check job names and paths: named character vectors
  expect_equal(job_getnames(jobs), c(toxic = "toxic"))
  expect_equal(job_getpaths(jobs), c(toxic = NA_character_))

  # verification tests (here because that was the original file structure)
  expect_true(verify_jobexists("toxic", jobs, strict = FALSE))
  expect_true(verify_jobmissing("hitmebaby", jobs, strict = FALSE))

  # add a second job that does have a path
  jobs[["hitmebaby"]] <- suppressWarnings(new_job(
    jobname = "hitmebaby",
    description = "another song",
    owner = "britney",
    path = loc
  ))
  job_write(jobs)

  # check job read has the correct info
  expect_equal(job_read(), jobs)

  # check job names and paths: named character vectors
  expect_equal(job_getnames(jobs), c(toxic = "toxic", hitmebaby = "hitmebaby"))
  expect_equal(job_getpaths(jobs), c(toxic = NA_character_, hitmebaby = loc))

  # verification tests (here because that was the original file structure)
  expect_true(verify_jobexists("toxic", jobs, strict = FALSE))
  expect_true(verify_jobexists("hitmebaby", jobs, strict = FALSE))

})

# doesn't test the RStudio functionality
test_that("job_getcurrent works", {

  jobs <- job_read()
  wd <- getwd()
  setwd("~")

  expect_error(job_getcurrent(jobs), "could not detect current job")

  setwd(loc)
  expect_equal(job_getcurrent(jobs), c(hitmebaby = "hitmebaby"))

  setwd(wd)

})


test_that("job_pathcheck works", {

  expect_true(job_pathcheck("toxic", loc))
  expect_warning(job_pathcheck("toxic", "dfkjaskldjfha"), "but does not exist")

})

