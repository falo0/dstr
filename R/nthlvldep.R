#' Get All Depper Level Dependencies
#'
#' This function inputs a list of packages, regarded to as fist level
#' dependencies. It outputs the dependencies, dependencies of dependencies, ...
#' of these first level dependencies. There are various options for the type of
#' the output.
#' @param pkg A list of packages from which we want to know the further
#' dependencies
#' @param outtype Possible output types:
#' edge_list: An edge list, e.g. to be used for network plots
#' allPkgs: An overview of all packages that are eventually loaded. No further
#' structure visible
#' list: More detailed than allPKs, it's a list tha containns all packages per
#' first level dependency
#' unique_list: like "list", just excluding all packages, that are eventually
#' also loaded by another package in firstlvldep. This way you can see which
#' dependencies will be removed completely if you remove a certain first level
#' dependency (a package that you import).
#' tree: Detailed information about which package depends on which, represented
#' in a data frame that is showing a tree structure.
#' @export


nthlvldep <- function(pkg, outtype = "edge_list"){

  if(!is.null(pkg)){
    stopifnot(is.character(pkg))
  }

  #find the base packages
  base.pkgs <- rownames(installed.packages(priority="base"))

  #if pkg not assigned use packages activated
  if(is.null(pkg)){
    used.pkgs<-(.packages())
    pkg <- used.pkgs[used.pkgs %in% base.pkgs == F]
  }

  bigmat <- available.packages()  #have to check if Git Hub repository
  deplevels <- c("Imports","Depends")

  edge.list <- function(pkg, recursive = T,
                        which = c("Imports","Depends"), includebasepkgs = F){


    #create a vector of all needed packages (the vertices)
    all.pkgs <- tools::package_dependencies(pkg, recursive = recursive, which = c("Imports","Depends"), db=bigmat)
    all.pkgs <- as.vector(unique(unlist(lapply(all.pkgs,rbind))))
    if (length(all.pkgs)==0){
      if (length(pkg)>1){
        stop("Your packages have no further dependencies")
      } else {
        stop("Your package has no further dependencies")
      }
    }
    all.pkgs <- c(pkg,all.pkgs)


    #create a function which takes a pkgname as input and returns a dataframe with the
    #depending firstlevel packages in column 2 (end.vertex). The 3rd column "dependencies"
    #is an additional information for the edgelist, and shows the type of the connection.

    level_pkg <- function(x){
      results <- data.frame("start" = character(),"end" = character())

      for (i in 1:length(deplevels)){
        pkgs <- unlist(tools::package_dependencies(x, recursive = F , which = deplevels[i], db=bigmat))

        results <- rbind(results, data.frame("start" = rep(x, length(unlist(pkgs))),
                                             "end" =  c(unlist(pkgs))))
      }
      return(results)
    }

    # combine the small dataframes for each package to one edgelist
    if (recursive == T){
      result.df <-do.call(rbind,lapply(all.pkgs, level_pkg))
    } else {

      result.df <-do.call(rbind,lapply(pkg, level_pkg))

    }


    #if necessary delete all connections to base packages
    if ((includebasepkgs == F)) {
      result.df <- subset(result.df, !(result.df[, 1] %in% base.pkgs))
      result.df <- subset(result.df, !(result.df[, 2] %in% base.pkgs))
      all.pkgs <- subset(all.pkgs, !(all.pkgs %in% base.pkgs))
    }

    rownames(result.df) <- NULL

    #return a list of the edgelist in form of a dataframe and the vector of all
    #packages needed (this is maybe not necessary and can be deleted)
    return(list(result.df, all.pkgs))
  }
  result <- edge.list(pkg)

  if(length(outtype) == 1){
    if(outtype == "edge_list"){
      return(result[[1]])
    } else if (outtype == "allPkgs"){
      return(result[[2]])
    } else {
      stop("outtype has to be at least on of the following (possibly a vector of
           several): edge_list, allPkgs")
    }

  } else if (length(outtype) == 1){
    outlist <- list()
    if(outtype == "edge_list"){
      outlist[[length(outlist)+1]] <- result[[1]]
    } else if (outtype == "allPkgs"){
      outlist[[length(outlist)+1]] <- result[[2]]
    } else {
      stop("outtype has to be at least on of the following (possibly a vector of
           several): edge_list, allPkgs")
    }

    return(outlist)

  } else {
    #outtype has a length of 0
    stop("Your outtype input has length 0")
  }

}














