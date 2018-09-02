#' Dependency Summary
#'
#' This function gives a summary of the dependency structure of a given package
#' and points out opportunities to eliminate depdendenceis completely.
#'
#' The default assumption is that there is an R package in the current working
#' directory and that the dependencies to be analyzed are given in the DESCRIPTION
#' file. Use the parameters ‘githublink’ and/or 'pkg' to alter the package/s
#' to be analyzed.
#' @param githublink A link to a github repository of an R package.
#' @param pkg A list of packages from which we want to know the further
#' dependencies. This list will be added to the first level dependencies
#' of a given package on github it githublink is set.
#' @param includebasepkgs Whether to include base packages in the analysis.
#' @export

dstr_summary <- function(githublink = NULL, pkg = NULL, includebasepkgs = F){

  writeLines("Loading...\n")

  data <- dstr_data(githublink, pkg, c("root_package", "unique_list_inclusive",
                                       "all_packages", "list"),
                    includebasepkgs = includebasepkgs)
  uniquelist <- data[[2]]
  allpkg <- data[[3]]
  dlist <- data[[4]]


  if(is.null(githublink) & !is.null(pkg)){
    writeLines(paste0("--- [dstr] DEPENDENCY STRUCTURE ANALYSIS OF { ",
               paste0(pkg, collapse = ", ")," } ---"))
  } else if(is.null(pkg) & !is.null(githublink)){
    writeLines(paste0("--- [dstr] DEPENDENCY STRUCTURE ANALYSIS OF '", data[[1]],"' ---"))
  } else if(!is.null(pkg) & !is.null(githublink)){
    writeLines(paste0("--- [dstr] DEPENDENCY STRUCTURE ANALYSIS OF '", data[[1]],
                      " + { ",paste0(pkg, collapse = ", ")," }' ---"))
  } else {
    writeLines(paste0("--- [dstr] DEPENDENCY STRUCTURE ANALYSIS OF '", data[[1]],"' ---"))
  }

  if(!includebasepkgs){
    writeLines("Base packages are ignored in this analysis.")
  }
  writeLines("\n")

  if(is.null(pkg)){
    writeLines(paste0("First Level Dependencies (Packages Found In The DESCRIPTION File) (",
                      length(data[[2]]),")"))
  } else if (!is.null(githublink)){
    # both githublink and pkg are set
    writeLines(paste0("First Level Dependencies (Packages Found In The DESCRIPTION File)",
                      "\n+ Input Packages From The 'pkg' Parameter (",
                      length(data[[2]])," In Total)"))
  } else {
    # only pkg is set
    # writeLines(paste0("Input Packages From The 'pkg' Parameter (",
    #                  length(data[[2]]),")"))
    writeLines(paste0("First Level Dependencies (Packages Found In The DESCRIPTION File/s",
                      "\nOf The Specified Package/s In The 'pkg' Parameter) (",
                      length(data[[2]]),")"))
  }

  paragraphsep___ <- paste0(rep("-", 80), collapse = "")
  writeLines(paragraphsep___)
  #writeLines(paste(names(data[[2]]), collapse = ", "))
  print(names(data[[2]]))

  writeLines("\n")

  writeLines(paste("All", length(data[[3]]), "Eventually Loaded Packages (Dependencies Of Dependencies Of...)"))
  writeLines(paragraphsep___)
  print(data[[3]])

  writeLines("\n")

  writeLines("Opportunities To Reduce Dependencies (Iterating Through All First Level Dependencies)")
  writeLines(paragraphsep___)
  # Sort the list so that packages with most dependencies are first in list
  uniquelist <- uniquelist[names(sort(sapply(uniquelist, length),
                                      decreasing = T))]

  for (j in 1:length(uniquelist)){
    if(length(uniquelist[[j]]) > 1){
    writeLines(paste0("If you remove '", names(uniquelist)[j],
                 "' you will remove the following ", length(uniquelist[[j]]),
                                                           " packages completely:"))
      print(uniquelist[[j]])
    } else if (length(uniquelist[[j]]) == 1){
      writeLines(paste0("If you remove '", names(uniquelist)[j],
                        "' you will remove the following ", length(uniquelist[[j]]),
                        " package completely:"))
      print(uniquelist[[j]])
    } else {
      #length(uniquelist[[j]]) == 0
      #sought <- "shiny"
      sought <- names(uniquelist)[j]

      soughtinlist <- sapply(dlist, function(x) sought %in% x)
      loaders <- names(soughtinlist)[soughtinlist]

      writeLines(paste0("If you remove '", names(uniquelist)[j],"' you will remove 0 other packages and also not '", names(uniquelist)[j],
                   "' istelf because it is a deeper level dependency from the following first level dependencies:"))
      print(loaders)
    }

    writeLines("\n")
  }

  writeLines("Shared Dependencies / Hard To Remove")
  writeLines(paragraphsep___)

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


    #sapply(shareddeps, function(x){all(x == y)})
    collapsed_loaded <- lapply(unique_loaders,
                               function(y) names(shareddeps)[sapply(shareddeps,
                                                     function(x){identical(x, y)})])
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
}
