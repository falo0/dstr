#' Dependency Summary
#'
#' This function gives a summary of the dependency structure of a given package
#' and points out opportunities to eliminate depdendenceis completely.
#' @param githublink A link to a github repository of an R package
#' @param pkg A list of packages from which we want to know the further
#' dependencies. This list will be added to the first level dependencies
#' of a given package on github it githublink is set
#' @export

dstrsummary <- function(githublink = NULL, pkg = NULL){
  #pkg <- NULL
  #githublink <- "tidyverse/ggplot2"
  #githublink <- "Stan125/GREA"

  data <- nthlvldep(githublink, pkg, c("firstlvlpkgs", "allpackages", "uniquelist", "list"))


  print("First Level Packages:")
  print(data[[1]])
  print("--------------------")

  print("All Eventually Loaded Packages (dependencies of dependencies of...):")
  print(data[[2]])
  print("--------------------")


  print("Opportunities To Eliminate Packages:")
  uniquelist <- data[[3]]
  dlist <- data[[4]]

  for (j in 1:length(uniquelist)){
    print(paste0("If you remove '", names(uniquelist)[j], "' you will remove completely:"))

    if(length(uniquelist[[j]]) == 0){

      firstlvlpkgname <- names(dlist)[j]
      loaderlist <- list()
      for (i in 1:length(dlist)){
        allother <- dlist[-i]
        loader <- character()
        for(i in 1:length(allother)){
          if(firstlvlpkgname %in% allother[[i]]){
            loader[length(loader)+1] <- names(allother)[i]
          }
        }
        loaderlist <- loader
      }

      if(length(loaderlist) > 0){
        print(paste0("Zero other packages and also not '", names(uniquelist)[j],
                     "' istelf because it is a deeper level depencendy from the
                     following first level dependencies: ", loaderlist))
      } else {
        print(paste0("Zero other packages, but you will remove '",
                     names(uniquelist)[j], "' itself"))
      }


    } else {
      print(c(names(uniquelist)[j], uniquelist[[j]]))
    }
  }



  #writeLines(c("First Level Packages",
  #             data[[1]],
  #             "All Eventually Loaded Packages (dependencies of dependencies of...)"))
}
