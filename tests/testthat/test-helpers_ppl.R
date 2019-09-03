
# reset home to a tempory directory
options(workbch.home = tempdir())

# expected structure of an empty person list
empty_tbl <- tibble::tibble(
  fullname = character(0),
  nickname = character(0)
)

test_that("empty tibble is returned when no ppl file exists", {
  expect_false(file.exists(ppl_file()))
  expect_equal(ppl_read(), empty_tbl)
})

# add a person, no defaults
set_person(
  fullname = "Danielle Navarro",
  nickname = "danielle"
)

# what the tibble should now look like
filled_tbl <- tibble::tibble(
  fullname = "Danielle Navarro",
  nickname = "danielle"
)

test_that("file is created on set_person", {
  expect_true(file.exists(ppl_file()))
  expect_equal(ppl_read(), filled_tbl)
})


test_that("ppl_fullname works", {

  # check the output
  expect_equal(ppl_fullname(character(0)), character(0))
  expect_equal(ppl_fullname("danielle"), "Danielle Navarro")
  expect_equal(ppl_fullname("Danielle Navarro"), "Danielle Navarro")
  expect_equal(
    object = suppressWarnings(ppl_fullname(c("danielle", "dani"))),
    expected = c("Danielle Navarro", "dani")
  )

  # check for warnings
  expect_warning(ppl_fullname("dani"), "is not a known nick name")

})

