context("does dstr_data perform right?")

library("dstr")
library("testthat")

result <- dstr_data(pkg="dplyr", recursive=T, includebasepkgs = F, outtype="all")
result2 <- dstr_data(pkg="dplyr", recursive=T, includebasepkgs = F, outtype="tree")
result3 <- dstr_data(pkg="dplyr", recursive=T, includebasepkgs = F, outtype="edgelist")
result4 <- dstr_data(pkg="dplyr", recursive=F, includebasepkgs = F, outtype="all")
result5 <- dstr_data(pkg="dplyr", recursive=F, includebasepkgs = F, outtype="list")

base_pkgs <- rownames(installed.packages(priority="base"))

test_that("dstr_data returns no base_pkgs when includebasepkgs=F",{

  #function which checks for any base pkgs
  check_base <- function(x){
    any(base_pkgs %in% x)
  }

  expect_false(check_base(result))
  expect_false(any(apply(result2, 2, check_base) ==T))
  expect_false(any(apply(result3, 2, check_base) ==T))

})


test_that("recursive = T is working", {
  expect_true(length(result) > length(result4))
  })


test_that("output is equal to tools package_dependencies()",{

  true_deps <- as.vector(unlist(tools::package_dependencies("dplyr", db=bigmat, which=c("Depends","Imports"),
                                          recursive=T)))
 true_deps <- true_deps[which(!true_deps %in% base_pkgs)]

 expect_true(length(true_deps) == (length(result)))
 expect_true(all(true_deps %in% result))
  })


test_that("output is of correct type",{

  expect_true(class(result) == "character")
  expect_true(class(result2) == "data.frame")
  expect_true(class(result3) == "data.frame")
  expect_true(class(result5) == "list")

})


rm(result)
rm(result2)
rm(result3)
rm(result4)
rm(result5)
rm(base_pkgs)
