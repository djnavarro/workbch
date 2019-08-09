
# reset home to a tempory directory
options(workbch.home = tempdir())

# expected structure of an empty person list
empty_tbl <- tibble::tibble(
  fullname = character(0),
  nickname = character(0),
  default = logical(0)
)

test_that("empty tibble is returned when no ppl file exists", {
  expect_false(file.exists(ppl_file()))
  expect_equal(ppl_read(), empty_tbl)
})

# add a person, no defaults
set_person(
  fullname = "Danielle Navarro",
  nickname = "danielle",
  make_default = FALSE
)

# what the tibble should now look like
filled_tbl <- tibble::tibble(
  fullname = "Danielle Navarro",
  nickname = "danielle",
  default = FALSE
)

test_that("file is created on set_person", {
  expect_true(file.exists(ppl_file()))
  expect_equal(ppl_read(), filled_tbl)
})


test_that("ppl_get_fullname works", {

  # check the output
  expect_equal(ppl_get_fullname(character(0)), character(0))
  expect_equal(ppl_get_fullname("danielle"), "Danielle Navarro")
  expect_equal(
    suppressWarnings(ppl_get_fullname(c("danielle", "dani"))),
    c("Danielle Navarro", "dani")
  )

  # check for warnings
  expect_warning(ppl_get_fullname("Danielle Navarro"), "has a known nick name")
  expect_warning(ppl_get_fullname("dani"), "is not a known nick name")

})

test_that("ppl_get_default works", {

  expect_error(ppl_get_default(), "no default person is set")

  set_person(
    fullname = "Britney Spears",
    nickname = "britney",
    make_default = TRUE
  )

  expect_equal(ppl_get_default(), "Britney Spears")

})

