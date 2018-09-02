#' dstr: A package for package dependency structure analysis.
#'
#' The dstr package provides 4 functions:
#' - dstr()
#' - dstr_data()
#' - plotdstr()
#' - edges2tree()
#'
#' With these functions it is possible to detect unnecessary loaded packages or
#' shared dependencies between packages.
#'
#' plotdstr() provides a dependency graph to the user.
#' dstr_data() delivers different data outputs of package dependencies.
#' dstr() is some kind of summary function which shows the user optimization
#' possibilities for his package regarding dependencies.
#' edges2tree transforms an edgelist into a tree data.frame. This is sometimes
#' more useful in understanding the sequence of loading the packages.
#'
#'
#' @docType package
#' @name dstr_package
NULL
#> NULL
