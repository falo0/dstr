context("endresult correct?")
library("dstr")
library("igraph")

test_that("network object is of type igraph", {
  network1 <- dstr_network(pkg= c("igraph"), recursive = T, includebasepkgs = F)

  network2 <- dstr_network(githublink = "https://github.com/tidyverse/ggplot2",
                           recursive = F, includebasepkgs = T)



  allnetworks <- list(network1,network2)
  expect_match(unlist(lapply(allnetworks, class)), "igraph",all = T)
})

# don't test too many possibilities. Tests have to run fast, under a minute.

