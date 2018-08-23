#' First Level Dependencies
#'
#' This function returns all first level packag dependencies from a package on github.
#' @param githublink A link to the package repository or directly to the description file
#' @param localdir A directory to a locally stored package or directly to a DESCRIPTION file
#' @export
#' @examples
#' firstlvldep("https://github.com/tidyverse/ggplot2")
#' firstlvldep(localdesdir = "~/Documents/myPackage/DESCRIPTION")


firstlvldep <- function(githublink = NULL, localdir = NULL, includeRootPkg = F){
  if(!is.null(githublink)){
    res <- helper_pkgname_rawlink(githublink)
    pkgname <- res[1]
    descfile <- readLines(res[2])
  } else {
    pkgname <- "RootPKG"

    if (file_test("-f", localdir)){
      #print("hi")
      #print(localdir)
      #print(getwd())
      # the localdir leads directly to a file (asumption: a description file)
      descfile <- readLines(localdir)
      #print(descfile)
    } else {
      #print("unten")
      #print(localdir)
      # localdir leads to a folder (assumption: folder of the package)
      descfile <- readLines(paste0(localdir, "/DESCRIPTION"))
    }

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

  if(includeRootPkg){
    return(list(lvl0 = pkgname, lvl1 = c(depends, imports)))
  } else {
    return(c(depends, imports))
  }
}
