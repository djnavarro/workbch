test_that("verify_onestring works", {
  msg <- "must be character and length 1"
  expect_error(verify_onestring(1), msg)
  expect_error(verify_onestring(NA), msg)
  expect_error(verify_onestring(NULL), msg)
  expect_error(verify_onestring(c("a", "b")), msg)
  expect_error(verify_onestring(list("a")), msg)
  expect_error(verify_onestring(matrix("a")), msg)
  expect_error(verify_onestring(matrix(character(0L))), msg)
  expect_error(verify_onestring(TRUE), msg)
})
