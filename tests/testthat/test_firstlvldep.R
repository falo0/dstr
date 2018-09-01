context("does firstlvldep() work correctly?")
library("dstr")
# ---------- firstlvldep() Tests ---------- #

test_that("firstlvldep returns a character vector when includeRootPkg = F", {
  expect_true(is.character(firstlvldep("https://github.com/tidyverse/ggplot2")))
})

test_that("firstlvldep returns a list that has 2 elements of type character when includeRootPkg = T", {
  res_list <- firstlvldep("https://github.com/tidyverse/ggplot2", includeRootPkg = T)
  expect_true(is.character(res_list[[1]]))
  expect_true(is.character(res_list[[2]]))
  expect_equal(length(res_list), 2)

  # just to find out what happens
  # expect_equal(3, 4)
})



test_that("firstlvldep parses test_DESCRIPTION_X correctly", {
  res1 <- firstlvldep(localdir = "test_files/test_DESCRIPTION_1")
  res2 <- firstlvldep(localdir = "test_files/test_DESCRIPTION_2")
  res3 <- firstlvldep(localdir = "test_files/test_DESCRIPTION_3")

  proper_result <- c("digest", "grid", "gtable", "lazyeval", "MASS", "mgcv",
                     "plyr", "reshape2", "rlang", "scales", "stats", "tibble",
                     "viridisLite", "withr")

  expect_equal(res1, proper_result)
  expect_equal(res2, proper_result)
  expect_equal(res3, proper_result)
})






