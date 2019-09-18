
loc <- tempdir()
options(workbch.home = loc)

test_that("sentinel_write works", {

  s <- file.path(loc, ".workbch")
  if(file.exists(s)) { file.remove(s) }

  sentinel_write(dir = loc, jobname = "toxic", idstring = "abcdeABCDE")

  expect_true(file.exists(s))
  expect_equal(readLines(s), c("toxic", "abcdeABCDE"))

})


test_that("sentinel_missing works", {

  # create path
  jobdir <- file.path(loc, "hitmebaby")
  if(!dir.exists(jobdir)) dir.create(jobdir)

  # write the job with no sentinel file
  jobs <- list()
  jobs[["hitmebaby"]] <- new_job(
    jobname = "hitmebaby",
    description = "a song",
    owner = "britney",
    path = jobdir,
    sentinel = FALSE
  )
  job_write(jobs)

  # expect it to detect the missing sentinel
  expect_equal(sentinel_missing(), "hitmebaby")

  # now write the sentinel
  sentinel_write(dir = jobdir, jobname = "hitmebaby",
                 idstring = jobs[["hitmebaby"]]$idstring)

  # expect nothing missing
  expect_length(sentinel_missing(), 0)

})
