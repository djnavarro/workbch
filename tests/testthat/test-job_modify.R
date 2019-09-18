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

# create a job object directly
job1 <- list()
job1[["hitmebaby"]] <- new_job(
  jobname = "hitmebaby",
  description = "a song",
  owner = "britney",
  status = "masked",
  priority = 2,
  path = jobdir,
  tags = "awesome",
  sentinel = FALSE
)

# write a different entity to file
job_create(
  jobname = "toxic",
  description = "another song",
  owner = "not britney"
)

test_that("job_modify works", {

  # try modifying everything at once to transform toxic
  # into hitmebaby
  job_modify(
    jobname = "toxic",
    newname = "hitmebaby",
    description = "a song",
    owner = "britney",
    status = "masked",
    priority = 2,
    path = jobdir,
    tags = "awesome"
  )

  # the idstring will be different, overwrite
  job2 <- job_read()
  job1[[1]]$idstring <- job2[[1]]$idstring

  # check identical
  expect_equal(job2, job1)

  # add a url
  job_modify(
    jobname = "hitmebaby",
    url = "github | http://nope.com"
  )

  # check that the correct information is written
  job3 <- job_read()
  expect_equal(job3[["hitmebaby"]]$urls$site, "github")
  expect_equal(job3[["hitmebaby"]]$urls$link, "http://nope.com")

  # SOMETHING WEIRD HERE

  # # delete a url
  # job_modify(
  #   jobname = "hitmebaby",
  #   url = "github"
  # )
  #
  # # chech that it is deleted
  # job3 <- job_read()
  # expect_length(job3[["hitmebaby"]]$urls$site, 0)
  # expect_length(job3[["hitmebaby"]]$urls$link, 0)


  # delete the job
  expect_equal(job_home("hitmebaby"), jobdir)
  job_modify("hitmebaby", delete = TRUE)
  expect_error(job_home("hitmebaby"), "No job exists with name")

})
