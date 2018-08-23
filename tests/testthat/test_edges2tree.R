# ---------- edges2tree() Tests ---------- #

# setwd("./tests/testthat")
# setwd("../..")

test_that("edges2tree correct output for GREA_edges input", {
  edges <- read.csv("test_files/GREA_edges.csv")
  lvl1deps <- c("shiny", "miniUI", "rstudioapi", "rio", "R.matlab", "DT")
  res <- edges2tree(edges, lvl1deps)
  proper_result <- read.csv("test_files/GREA_treeDF.csv", stringsAsFactors = F)

  # The NAs make the comparison difficult/impossible, so we impute them.
  res <- res[is.na(res)] <- 42
  proper_result <- proper_result[is.na(proper_result)] <- 42

  expect_true(res == proper_result)
})

test_that("edges2tree correct output for ggplot2_edges input", {
  edges <- read.csv("test_files/ggplot2_edges.csv")
  lvl1deps <- read.csv("test_files/ggplot2_lvl1deps.csv")
  res <- edges2tree(edges, lvl1deps)
  proper_result <- read.csv("test_files/ggplot2_treeDF.csv", stringsAsFactors = F)

  # The NAs make the comparison difficult/impossible, so we impute them.
  res <- res[is.na(res)] <- 42
  proper_result <- proper_result[is.na(proper_result)] <- 42

  expect_true(res == proper_result)
})

test_that("edges2tree correct output for simple_edges input", {
  edges <- read.csv("test_files/simple_edges.csv")
  lvl1deps <- "shiny"
  res <- edges2tree(edges, lvl1deps)
  proper_result <- read.csv("test_files/simple_treeDF.csv", stringsAsFactors = F)

  # The NAs make the comparison difficult/impossible, so we impute them.
  res <- res[is.na(res)] <- 42
  proper_result <- proper_result[is.na(proper_result)] <- 42

  expect_true(res == proper_result)
})




