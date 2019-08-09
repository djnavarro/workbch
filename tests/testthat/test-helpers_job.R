
# reset home to a tempory directory
loc <- tempdir()
options(workbch.home = loc)

test_that("job_file returns the correct path", {
  expect_equal(job_file(), file.path(loc, "workbch_jobs.json"))
})

test_that("reading and writing jobs does what it says", {

  # delete the job file if for some reason it exists
  if(file.exists(job_file())) {
    file.remove(job_file())
  }

  # check that job_read returns empty_job
  expect_equal(job_read(), empty_job())

  # create a job directly from the constructor
  jobs <- empty_job()
  jb <- new_job(jobname = "toxic",
                description = "a song",
                owner = "britney")
  jobs[["toxic"]] <- jb
  job_write(jobs)

  # check
  expect_equal(job_read(), jobs)

})
