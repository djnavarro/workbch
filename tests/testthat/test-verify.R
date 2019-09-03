test_that("verify_onestring works", {
  msg <- "must be character and length 1"

  # should fail
  expect_error(verify_onestring(1), msg)
  expect_error(verify_onestring(character(0L)), msg)
  expect_error(verify_onestring(NA), msg)
  expect_error(verify_onestring(NULL), msg)
  expect_error(verify_onestring(c("a", "b")), msg)
  expect_error(verify_onestring(list("a")), msg)
  expect_error(verify_onestring(matrix("a")), msg)
  expect_error(verify_onestring(matrix(character(0L))), msg)
  expect_error(verify_onestring(TRUE), msg)

  # should succeed
  expect_true(verify_onestring("asdf ^asf"))
})

test_that("verify_character works", {
  msg <- "must be character"

  # should fail
  expect_error(verify_character(1), msg)
  expect_error(verify_character(NA), msg)
  expect_error(verify_character(NULL), msg)
  expect_error(verify_character(list("a")), msg)
  expect_error(verify_character(matrix("a")), msg)
  expect_error(verify_character(matrix(character(0L))), msg)
  expect_error(verify_character(matrix(c("a","b"))), msg)
  expect_error(verify_character(TRUE), msg)

  # should succeed
  expect_true(verify_character(character(0L)))
  expect_true(verify_character(NA_character_))
  expect_true(verify_character("aasdfasdf"))
  expect_true(verify_character(c("asdfasdf", "a")))
})


test_that("verify_status works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_status(1), msg)
  expect_error(verify_status(c("active", "inactive")), msg)

  # check that invalid status string are not accepted
  msg <- "must be 'active', 'inactive'"
  expect_error(verify_status("blarhg"), msg)
  expect_error(verify_status("nactive"), msg)
  expect_error(verify_status("Active"), msg)
  expect_error(verify_status(" active"), msg)
  expect_error(verify_status("active "), msg)
  expect_error(verify_status("ac tive"), msg)

  # should succeed
  expect_true(verify_status("active"))
  expect_true(verify_status("inactive"))
  expect_true(verify_status("complete"))
  expect_true(verify_status("abandoned"))
  expect_true(verify_status("masked"))

})


test_that("verify_priority works", {

  # check that invalid priorities throw errors
  msg <- "job priority must be a positive integer"
  expect_error(verify_priority(0), msg)
  expect_error(verify_priority(-1), msg)
  expect_error(verify_priority(NaN), msg)
  expect_error(verify_priority(1.2), msg)
  expect_error(verify_priority("a"), msg)
  expect_error(verify_priority(1:2), msg)
  expect_error(verify_priority(NULL), msg)
  expect_error(verify_priority(Inf), msg)
  expect_error(verify_priority(NA_integer_), msg)
  expect_error(verify_priority(NA), msg)

  # should succeed
  expect_true(verify_priority(1))
  expect_true(verify_priority(2))
  expect_true(verify_priority(3))

})

test_that("verify_description works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_description(1), msg)
  expect_error(verify_description(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_description("a job to do something"))

})

test_that("verify_site works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_site(1), msg)
  expect_error(verify_site(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_site("gitblah"))

})

test_that("verify_link works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_link(1), msg)
  expect_error(verify_link(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_link("https://gitblub.com/fuser/depot"))

})

test_that("verify_nickname works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_nickname(1), msg)
  expect_error(verify_nickname(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_nickname("britney"))

})

test_that("verify_fullname works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_fullname(1), msg)
  expect_error(verify_fullname(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_fullname("Britney Spears"))

})


test_that("verify_owner works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_owner(1), msg)
  expect_error(verify_owner(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_owner("britney"))

})


test_that("verify_jobname works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_jobname(1), msg)
  expect_error(verify_jobname(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_jobname("toxic"))

})


test_that("verify_path works", {

  # cursory check that verify_onestring is called
  msg <- "must be character and length 1"
  expect_error(verify_path(1), msg)
  expect_error(verify_path(c("blah blah", "blah")), msg)

  # expect success
  expect_true(verify_path("~/GitHub/"))

})

