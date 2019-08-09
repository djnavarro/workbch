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