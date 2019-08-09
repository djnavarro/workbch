library(testthat)
library(workbch)

wbh <- getOption("workbch.home")

test_check("workbch")

options(workbch.home = wbh)

