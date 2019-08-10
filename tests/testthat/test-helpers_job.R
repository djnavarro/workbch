
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

  # check that job_read returns empty_job
  jobs <- job_read()
  expect_equal(jobs, empty_job())

  # while we're here, lets try to read empty jobnames and paths
  expect_equal(job_getnames(jobs), character(0))
  expect_equal(job_getpaths(jobs), character(0))

  # create a job directly from the constructor
  jobs <- empty_job()
  jb <- new_job(jobname = "toxic",
                description = "a song",
                owner = "britney")
  jobs[["toxic"]] <- jb
  job_write(jobs)

  # check job read has the correct info
  expect_equal(job_read(), jobs)

  # check job names and paths: named character vectors
  expect_equal(job_getnames(jobs), c(toxic = "toxic"))
  expect_equal(job_getpaths(jobs), c(toxic = NA_character_))

  # add a second job that does have a path
  jb <- new_job(jobname = "hitmebaby",
                description = "another song",
                owner = "britney",
                path = loc)
  jobs[["hitmebaby"]] <- jb
  job_write(jobs)

  # check job read has the correct info
  expect_equal(job_read(), jobs)

  # check job names and paths: named character vectors
  expect_equal(job_getnames(jobs), c(toxic = "toxic", hitmebaby = "hitmebaby"))
  expect_equal(job_getpaths(jobs), c(toxic = NA_character_, hitmebaby = loc))

})
