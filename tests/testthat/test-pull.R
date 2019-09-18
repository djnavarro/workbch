
# reset home to a tempory directory
loc <- tempdir()
options(workbch.home = loc)

test_that("pull functions work", {

  jobs <- NULL

  # pull from empty jobnames and paths
  expect_equal(pull_jobnames(jobs), character(0))
  expect_equal(pull_jobpaths(jobs), character(0))
  expect_equal(pull_jobinfo(jobs), tibble::tibble())

  jobs <- list()
  jobs[["toxic"]] <- workbch:::new_job(
    jobname = "toxic",
    description = "a song",
    owner = "britney",
    sentinel = FALSE)

  # check job names and paths: named character vectors
  expect_equal(pull_jobnames(jobs), c(toxic = "toxic"))
  expect_equal(pull_jobpaths(jobs), c(toxic = NA_character_))
  expect_named(pull_jobinfo(jobs), c("jobname", "path", "idstring"))
  expect_equal(nrow(pull_jobinfo(jobs)), 0)

  # add a second job that does have a path
  jobs[["hitmebaby"]] <- new_job(
    jobname = "hitmebaby",
    description = "another song",
    owner = "britney",
    sentinel = FALSE,
    path = loc
  )

  # check job names and paths: named character vectors
  expect_equal(pull_jobnames(jobs), c(toxic = "toxic", hitmebaby = "hitmebaby"))
  expect_equal(pull_jobpaths(jobs), c(toxic = NA_character_, hitmebaby = loc))
  expect_named(pull_jobinfo(jobs), c("jobname", "path", "idstring"))
  expect_equal(nrow(pull_jobinfo(jobs)), 1)
  expect_equal(pull_jobinfo(jobs)$jobname, "hitmebaby")
  expect_equal(pull_jobinfo(jobs)$path, loc)
  expect_equal(pull_jobinfo(jobs)$idstring, jobs[["hitmebaby"]]$idstring)


})



