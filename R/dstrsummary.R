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

  #pkg <- "miniCRAN"
  #pkg <- c("ggplot2", "data.table")
  #pkg <- NULL
  #githublink <- NULL
  #githublink <- "tidyverse/ggplot2"
  #githublink <- "Stan125/GREA"

  data <- nthlvldep(githublink, pkg, c("rootpackage", "unique_list_inclusive", "allpackages", "list"))
  uniquelist <- data[[2]]
  allpkg <- data[[3]]
  dlist <- data[[4]]


  if(is.null(githublink)){
    writeLines("--- DEPENDENCY ANALYSIS ---\n")
  } else if(is.null(pkg)){
    writeLines(paste0("--- '", data[[1]], "' DEPENDENCY ANALYSIS ---\n"))
  } else {
    writeLines(paste0("--- '", data[[1]], " + pkg", "' DEPENDENCY ANALYSIS ---\n"))
  }

  #writeLines("###############")

  writeLines(paste0("First Level Packages (", length(data[[2]]),")"))
  writeLines("---------------------------------------------------------")
  #writeLines(paste(names(data[[2]]), collapse = ", "))
  print(names(data[[2]]))

  writeLines("\n")

  writeLines(paste("All", length(data[[3]]), "Eventually Loaded Packages (Dependencies Of Dependencies Of...)"))
  writeLines("---------------------------------------------------------")
  print(data[[3]])

  writeLines("\n")

  writeLines("Opportunities To Reduce Dependencies")
  writeLines("---------------------------------------------------------")
  # Sort the list so that packages with most dependencies are first in list
  uniquelist <- uniquelist[names(sort(sapply(uniquelist, length),
                                      decreasing = T))]

  for (j in 1:length(uniquelist)){
    writeLines(paste0("If you remove '", names(uniquelist)[j],
                 "' you will remove the following ", length(uniquelist[[j]]),
                                                           " packages completely:"))

    if(length(uniquelist[[j]]) == 0){
      #sought <- "shiny"
      sought <- names(uniquelist)[j]

      soughtinlist <- sapply(dlist, function(x) sought %in% x)
      loaders <- names(soughtinlist)[soughtinlist]

      writeLines(paste0("Zero other packages and also not '", names(uniquelist)[j],
                   "' istelf because it is a deeper level depencendy from the
                   following first level dependencies:"))
      print(loaders)
    } else {
      #print(paste0("The following ", length(uniquelist[[j]]), " packages:"))
      print(uniquelist[[j]])
    }
    writeLines("\n")
  }

  writeLines("Shared Dependencies / Hard To Remove")
  writeLines("---------------------------------------------------------")

  shareddeps <- list()
  for(i in 1:length(allpkg)){

    soughtinlist <- sapply(dlist, function(y) allpkg[i] %in% y)
    loaders <- names(soughtinlist)[soughtinlist]
    if(length(loaders) > 1){
      shareddeps[[length(shareddeps)+1]] <- loaders
      names(shareddeps)[length(shareddeps)] <- allpkg[i]
    }

  }
  if(length(shareddeps) > 0){
    # Sort the list so that packages with most dependencies are first in list
    shareddeps <- shareddeps[names(sort(sapply(shareddeps, length),
                                        decreasing = T))]

    unique_loaders <- unique(shareddeps)
    collapsed_loaded <- lapply(unique_loaders,
                               function(y) names(shareddeps)[sapply(shareddeps,
                                                     function(x){all(x == y)})])
    for(i in 1:length(unique_loaders)){
      #writeLines(paste0("The packages '", paste(collapsed_loaded[[i]], collapse = ", "),
      #             "' are loaded by your (",length(unique_loaders[[i]]) ,") first level packages '",
      #             paste(unique_loaders[[i]], collapse = ", ", "'")))
      writeLines(paste0(length(unique_loaders[[i]]),
                        " first level packages ('",
                        paste0(unique_loaders[[i]], collapse = ", ", "'"),
                        ") depend on the following packages:"))
      print(collapsed_loaded[[i]])
      writeLines("\n")
    }
  } else {
    writeLines("You don't have shared dependencies, e.g. none of the ulimatively loaded packages is loaded because of two or more first level packages.")
  }

  #writeLines(c("First Level Packages",
  #             data[[1]],
  #             "All Eventually Loaded Packages (dependencies of dependencies of...)"))
}
