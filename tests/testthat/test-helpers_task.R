
# reset home to a tempory directory and
# delete the job file if it exists
loc <- tempdir()
options(workbch.home = loc)
if(file.exists(job_file())) {
  file.remove(job_file())
}

test_that("task helpers work", {

  jobs <- job_read()

  # empty tasks and ids when there are no jobs
  expect_equal(task_read(), empty_task())
  expect_equal(task_getids(jobs), numeric(0))

  # create a list of jobs directly from the constructor
  jobs <- list(toxic = new_job(
    jobname = "toxic", description = "a song", owner = "britney"
  ))
  job_write(jobs)

  # it shouldn't make any difference for task functions
  expect_equal(task_read(), empty_task())
  expect_equal(task_getids(jobs), numeric(0))

  # add a second job that has a task directly from the
  # constructor function
  tsk <- new_task(
    jobname = "hitmebaby",
    id = 5,
    description = "one more time",
    owner = "britney"
  )
  jobs[["hitmebaby"]] <- new_job(
    jobname = "hitmebaby",
    description = "another song",
    owner = "britney",
    tasks = tsk
  )
  job_write(jobs)

  expect_equal(task_read(), tsk)
  expect_equal(task_getids(jobs), 5)

})

