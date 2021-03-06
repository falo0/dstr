#' dstr: Package dependency structure analysis
#'
#' dstr's goal is to reveal the dependency structure of one or seceral given
#' package/s. E.g. it helps to find out which packages are eventually required
#' (all dependencies of dependencies of...), how they are connected, and how
#' easy or hard it would be to remove certain packages from the dependency
#' structure completely.
#'
#' The dstr package provides 4 functions:
#'
#' - dstr() gives a console output which shows the user descriptive information
#' about the dependency structure as well as opportunities to reduce dependencies.
#'
#' - plotdstr() provides a dependency graph to the user.
#'
#' - dstr_data() delivers different data outputs of package dependencies.
#'
#' - edges2tree() transforms an edgelist into a data.frame that represents a tree
#' structure. This is sometimes more useful in understanding the sequence in which
#' the packages are loaded.
#'
#'
#' @docType package
#' @name dstr_package
NULL
#> NULL
