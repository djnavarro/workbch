test_that("empty_url works", {
  expect_equal(
    empty_url(),
    new_url(
      site = character(0),
      link = character(0),
      verify = FALSE
    )
  )
})

test_that("idstring works", {
  set.seed(1)
  expect_equal(idstring(), "dMaHwQnrYG")
})

test_that("print method works", {

  tbl1 <- tibble::tibble(
    x = rnorm(50),
    y = rnorm(50)
  )

  tbl2 <- as_wkbch_tbl(tbl1)

  # same values different class
  expect_equivalent(tbl2, tbl1)
  expect_s3_class(tbl2, c("wkbch_tbl", class(tbl1)))

  # print method for wkbch_tbl fixes the output row
  out1 <- capture.output(print(tbl1, n = 100))
  out2 <- capture.output(print(tbl2))
  expect_identical(out1, out2)

  tbl3 <- tibble::tibble(
    x = rnorm(150),
    y = rnorm(150)
  )

  tbl4 <- as_wkbch_tbl(tbl3)

  # print method for wkbch_tbl fixes the output row
  out3 <- capture.output(print(tbl3, n = 100))
  out4 <- capture.output(print(tbl4))
  expect_identical(out3, out4)


})

test_that("split_url and split_tag work", {

  # make sure it calls verify_onestring
  expect_error(split_tags(1), "must be character and length 1")
  expect_error(split_url(1), "must be character and length 1")
  expect_error(split_tags(character(0)), "must be character and length 1")
  expect_error(split_url(character(0)), "must be character and length 1")
  expect_error(split_tags(c("a", "b")), "must be character and length 1")
  expect_error(split_url(c("a", "b")), "must be character and length 1")

  # make sure it splits
  expect_equal(split_tags(""), character(0))
  expect_equal(split_tags("|"), character(0))
  expect_equal(split_tags("aaa bbb"), "aaa bbb")
  expect_equal(split_tags("aaa | bbb"), c("aaa", "bbb"))
  expect_equal(split_tags(" aaa| "), "aaa")
  expect_equal(split_tags(" |bbb| "), "bbb")
  expect_equal(split_tags(" |aaa|||| bbb "), c("aaa", "bbb"))

  # make sure it splits
  expect_equal(split_url(""), character(0))
  expect_equal(split_url("|"), character(0))
  expect_equal(split_url("aaa bbb"), "aaa bbb")
  expect_equal(split_url("aaa | bbb"), c("aaa", "bbb"))
  expect_equal(split_url(" aaa| "), "aaa")
  expect_equal(split_url(" |bbb| "), "bbb")
  expect_equal(split_url(" |aaa|||| bbb "), c("aaa", "bbb"))


})



