
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

  # create a list of jobs directly from the constructor
  jobs <- suppressWarnings(list(toxic = new_job(
    jobname = "toxic", description = "a song", owner = "britney"
  )))
  job_write(jobs)

  # check job read has the correct info
  expect_equal(job_read(), jobs)

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

})

