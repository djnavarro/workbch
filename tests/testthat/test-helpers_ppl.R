
# reset home to a tempory directory
options(workbch.home = tempdir())

test_that("empty tibble is returned when no ppl file exists", {
  tbl <- tibble::tibble(
    fullname = character(0),
    nickname = character(0),
    default = logical(0)
  )
  expect_identical(ppl_read(), tbl)
})
