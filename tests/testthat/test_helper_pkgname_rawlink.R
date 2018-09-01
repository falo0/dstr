context("does the helpfer function work correctly?")
library("dstr")
# ---------- helper_pkgname_rawlink() Tests ---------- #

test_that("helper_pkgname_rawlink gets correct link and name from various input formats", {

  inputs <- c("https://raw.githubusercontent.com/falo0/dstr/master/DESCRIPTION",
              "https://github.com/falo0/dstr/blob/master/DESCRIPTION",
              "https://github.com/falo0/dstr/blob/master",
              "https://github.com/falo0/dstr",
              "falo0/dstr"
              )

  res <- lapply(inputs, helper_pkgname_rawlink)

  for(i in 1:length(res)){
    expect_equal(unname(res[[i]][1]), "dstr")
    expect_equal(unname(res[[i]][2]), "https://raw.githubusercontent.com/falo0/dstr/master/DESCRIPTION")
  }
})
