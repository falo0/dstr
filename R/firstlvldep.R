#' First Level Dependencies
#'
#' This function returns all first level packag dependencies from a package on github.
#' @param githublink A link to the package repository or directly to the description file
#' @param localdescdir A directory to a local DESCRIPTION file
#' @export
#' @examples
#' firstlvldep("https://github.com/tidyverse/ggplot2")
#' firstlvldep(localdesdir = "~/Documents/myPackage/DESCRIPTION")


firstlvldep <- function(githublink = NULL, localdescdir = NULL){
  if(!is.null(githublink)){
    descdir <- regmatches(githublink, gregexpr("^(.*.com)",
                                               githublink), invert = T)[[1]][2]
    descdir <- sub("\\/blob","", descdir)
    pkgname <- descdir
    if (!grepl("/master", descdir)){
      descdir <- paste0(descdir, "/master")
    } else{
      pkgname <- sub("\\/master","", pkgname)
    }
    if (!grepl("/DESCRIPTION", descdir)){
      descdir <- paste0(descdir, "/DESCRIPTION")
    } else{
      pkgname <- sub("\\/DESCRIPTION","", pkgname)
    }
    githublink <- paste0("https://raw.githubusercontent.com", descdir)

    pkgname <- regmatches(pkgname, gregexpr("^(.*\\/)",
                                    pkgname), invert = T)[[1]][2]
    descfile <- readLines(githublink)
  } else {
    pkgname <- "RootPKG"
    descfile <- readLines(localdescdir)
  }


  imports <- character()
  depends <- character()
  rangeOfImports <- F
  rangeOfDepends <- F
  for (i in 1:length(descfile)){
    if (startsWith(descfile[i], "Imports:")){
      imports <- descfile[i]
      rangeOfImports <- T
    } else if (rangeOfImports){
      if(grepl(":", descfile[i])){
        rangeOfImports <- F
      } else {
        imports[length(imports)+1] <- descfile[i]
      }

    }

    if (startsWith(descfile[i], "Depends:")){
      depends <- descfile[i]
      rangeOfDepends <- T
    } else if (rangeOfDepends){
      if(grepl(":", descfile[i])){
        rangeOfDepends <- F
      } else {
        depends[length(depends)+1] <- descfile[i]
      }
    }
  }

  parsefurther <- function(charVec){
    # seperate by comma
    charVec <- unlist(strsplit(charVec, ","))
    # remove version information
    charVec <- sub("\\((.*)", "", charVec)
    # remove tabs, returns, vertical tabs, new lines, backspaces, escapes, spaces
    # It needs 2 rounds in some cases for some reason.
    charVec <- sub("\\s+", "", charVec)
    charVec <- sub("\\s+", "", charVec)
    # remove title "Imports:" or "Depends:"
    charVec <- sub("^.*:","", charVec)
    # remove empty strings
    charVec <- charVec[charVec != ""]
    # remove the "R" entry because that's not a package
    charVec <- charVec[charVec != "R"]

    charVec
  }

  depends <- parsefurther(depends)
  imports <- parsefurther(imports)

  list(pkgname = pkgname, firstlvldep = c(depends, imports))
}
