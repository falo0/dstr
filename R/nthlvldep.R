#' Get All Deeper Level Dependencies
#'
#' This function inputs a list of packages, regarded to as fist level
#' dependencies. It outputs the dependencies, dependencies of dependencies, ...
#' of these first level dependencies. There are various options for the type of
#' the output.
#' @param githublink A link to a github repository of an R package
#' @param pkg A list of packages from which we want to know the further
#' dependencies. This list will be added to the first level dependencies
#' of a given package on github it githublink is set
#' @param outtype Possible output types:
#' \itemize{
#' \item edgelist: An edge list, e.g. to be used for network plots
#' \item edgelistdetailed: An edge list, e.g. to be used for network plots, including
#' information on the type of dependency
#' \item allpackages: An overview of all packages that are eventually loaded. No further
#' structure visible
#' \item list: More detailed than allPKs, it's a list tha containns all packages per
#' first level dependency
#' \item uniquelist: like "list", just excluding all packages, that are eventually
#' also loaded by another package in firstlvldep. This way you can see which
#' dependencies will be removed completely if you remove a certain first level
#' dependency (a package that you import).
#' \item tree: Detailed information about which package depends on which, represented
#' in a data frame that is showing a tree structure.
#' \item firstlvlpkgs: The vector of packages used as input. This is a combination
#' when both the githublink and the pkg parameter are set.
#' \item rootpackage: Whether to include the name of the package of the given
#' github link. Returns "Root Package" when no github link was given
#' }
#' @param includebasepkgs Whether to include base packages in the analysis or not
#' @param recursive Whether you want to look deeper than the second level of dependencies,
#' e.g. get all dependencies of dependencies of dependencies ...
#' @param which What type of depencencies are regarded
#' @export
#' @importFrom utils available.packages installed.packages read.csv


nthlvldep <- function(githublink = NULL, pkg = NULL, outtype,
                      includebasepkgs = F, recursive = T,
                      which = c("Imports", "Depends")){


  # ------- For testing only ---
  #githublink = "Stan125/GREA"
  #pkg = NULL
  #recursive = T
  #includebasepkgs = F
  #deplevels <- which ???
  # ----------------------------
  deplevels <- c("Imports","Depends")


  bigmat <- available.packages()  #have to check if Git Hub repository

  if(is.null(pkg) & is.null(githublink)){
    stop("You have to specify at least one of the two: pkg, githublink")
  } else if (!is.null(githublink) & is.null(pkg)){
    # only a github link and no vector of packages was given
    res <- firstlvldep(githublink, includeRootPkg = T)
    rootPkgName <- unname(res[[1]])
    pkg <- res[[2]]
  } else if (!is.null(githublink) & !is.null(pkg)){
    # both a githublink and a vector of packges where given, e.g. to test
    # what would happen if a certain package would also import the given vector
    # of packages
    res <- firstlvldep(githublink, includeRootPkg = T)
    rootPkgName <- unname(res[[1]])
    pkg <- unique(c(res[[2]], pkg))
  }

  #read the list of base packages
  base.pkgs <- rownames(installed.packages(priority="base"))

  #if pkg not assigned use packages activated
  if(is.null(pkg)){
    used.pkgs<-(.packages())
    pkg <- used.pkgs[used.pkgs %in% base.pkgs == F]
  }


  #create a vector of all needed packages (the vertices)
  frstlvllist <- tools::package_dependencies(pkg, recursive = recursive, which = deplevels, db=bigmat)
  all.pkgs <- unique(unname(unlist(frstlvllist)))

  all.pkgs <- c(pkg,all.pkgs)


  #create a function which takes a pkgname as input and returns a dataframe with the
  #depending firstlevel packages in column 2 (end.vertex). The 3rd column "dependencies"
  #is an additional information for the edgelist, and shows the type of the connection.

  level_pkg <- function(x){
    results <- data.frame("start" = character(),"end" = character(), "dependency" = character())

    for (i in 1:length(deplevels)){
      results <- data.frame("start" = character(),"end" = character(), "dependency" = character())

      for (i in 1:length(deplevels)){
        pkgs <-   unlist(tools::package_dependencies(x, recursive = F , which = deplevels[i], db=bigmat))

        results <- rbind(results, data.frame("start" = rep(x, length(unlist(pkgs))),
                                             "end" =  c(unlist(pkgs)),
                                             "dependencies" = rep(deplevels[i],length(unlist(pkgs)))))
      }
      return(results)
    }
  }

  # combine the small dataframes for each package to one edgelist
  if(length(all.pkgs > 0)){
    if (recursive == T){
      result.df <-do.call(rbind,lapply(all.pkgs, level_pkg))
    } else {

      result.df <-do.call(rbind,lapply(pkg, level_pkg))
    }
  } else {
    # There are no dependencies at all, return an empty data frame (and not NULL)
    result.df <- data.frame("start" = character(),"end" = character(), "dependency" = character())
  }
  row.names(result.df) <- NULL

  #if necessary delete all connections to base packages
  if ((includebasepkgs == F)) {
    result.df <- subset(result.df, !(result.df[, 1] %in% base.pkgs))
    result.df <- subset(result.df, !(result.df[, 2] %in% base.pkgs))
    all.pkgs <- subset(all.pkgs, !(all.pkgs %in% base.pkgs))
  }

  create_unique_list <- function(frstlvllist){
    if(length(frstlvllist) >= 1){
      uniquelist <- list()
      for(i in 1:length(frstlvllist)){
        everythingelse <- unname(unlist(frstlvllist[-i]))
        uniquelist[[i]] <- setdiff(frstlvllist[[i]], everythingelse)
      }
      names(uniquelist) <- pkg
      return(uniquelist)
    } else {
      return(frstlvllist)
    }
  }

  if(length(outtype) == 1){
    if(outtype == "edgelist"){
      return(result.df[,-3])
    } else if (outtype == "edgelistdetailed"){
      return(result.df)
    } else if (outtype == "allpackages"){
      return(all.pkgs)
    } else if (outtype == "list"){
      return(frstlvllist)
    } else if (outtype == "uniquelist"){
      return(create_unique_list(frstlvllist))
    } else if (outtype == "tree"){
      treeDF <- edges2tree(result.df[,-3], lvl1deps = pkg)
      # Optionally replace NA by "" so it is nicer to look at. Not sure if we should keep this
      treeDF[is.na(treeDF)] <- ""
      return(treeDF)
    } else if (outtype == "firstlvlpkgs"){
      return(pkg)
    } else if (outtype == "rootpackage"){
      if(exists("rootPkgName")){
        return(rootPkgName)
      } else {
        return("Root Package")
      }
    } else {
      stop("outtype has to be at least on of the following (possibly a vector of
           several): edgelist, edgelistdetailed, allpackages, list,
           uniquelist, tree, firstlvlpkgs, rootpackage")
    }



  } else if (length(outtype) > 1){

    #outtype <- c("edgelist", "allpackges")

    outlist <- list()
    for (i in 1:length(outtype)){
      if(outtype[i] == "edgelist"){
        outlist[[length(outlist)+1]] <- result.df[,-3]
      } else if (outtype[i] == "edgelistdetailed"){
        outlist[[length(outlist)+1]] <- result.df
      } else if (outtype[i] == "allpackages"){
        outlist[[length(outlist)+1]] <- all.pkgs
      } else if (outtype[i] == "list"){
        outlist[[length(outlist)+1]] <- frstlvllist
      } else if (outtype[i] == "uniquelist"){
        outlist[[length(outlist)+1]] <- create_unique_list(frstlvllist)
      } else if (outtype[i] == "tree"){
        treeDF <- edges2tree(result.df[,-3], lvl1deps = pkg)
        # Optionally replace NA by "" so it is nicer to look at. Not sure if we should keep this
        treeDF[is.na(treeDF)] <- ""
        outlist[[length(outlist)+1]] <- treeDF
      } else if (outtype[i] == "firstlvlpkgs"){
        outlist[[length(outlist)+1]] <- pkg
      } else if (outtype[i] == "rootpackage"){
        if(exists("rootPkgName")){
          outlist[[length(outlist)+1]] <-rootPkgName
        } else {
          outlist[[length(outlist)+1]] <-"Root Package"
        }
      } else {
        stop("outtype has to be at least one of the following (possibly a vector of
             several): edgelist, edgelistdetailed, allpackages, list,
             uniquelist, tree, firstlvlpkgs, rootpackage")
      }
    }

    names(outlist) <- outtype
    return(outlist)

  } else {
    #outtype has a length of 0
    stop("Your outtype input has length 0")
  }

}














