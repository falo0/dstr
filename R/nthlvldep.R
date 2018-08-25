#' Get All Depper Level Dependencies
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
#' edgelist: An edge list, e.g. to be used for network plots
#' edgelistdetailed: An edge list, e.g. to be used for network plots, including
#' information on the type of dependency
#' allpackages: An overview of all packages that are eventually loaded. No further
#' structure visible
#' list: More detailed than allPKs, it's a list tha containns all packages per
#' first level dependency
#' uniquelist: like "list", just excluding all packages, that are eventually
#' also loaded by another package in firstlvldep. This way you can see which
#' dependencies will be removed completely if you remove a certain first level
#' dependency (a package that you import).
#' tree: Detailed information about which package depends on which, represented
#' in a data frame that is showing a tree structure.
#' firstlvlpkgs: The vector of packages used as input. This is a combination
#' when both the githublink and the pkg parameter are set.
#' rootpackage: Whether to include the name of the package of the given
#' github link. Returns "Root Package" when no github link was given
#' @param includebasepkgs Whether to include base packages in the analysis or not
#' @param recursive Whether you want to look deeper than the second level of dependencies,
#' e.g. get all dependencies of dependencies of dependencies ...
#' @param which What type of depencencies are regarded
#' @export
#' @importFrom utils available.packages installed.packages read.csv


nthlvldep <- function(githublink = NULL, pkg = NULL, outtype,
                      includebasepkgs = F, recursive = T,
                      which = c("Imports", "Depends")){

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
    pkg <- unique(c(firstlvldep(githublink), pkg))
  }

  #read the list of base packages
  base.pkgs <- rownames(installed.packages(priority="base"))

  #if pkg not assigned use packages activated
  if(is.null(pkg)){
    used.pkgs<-(.packages())
    pkg <- used.pkgs[used.pkgs %in% base.pkgs == F]
  }

  bigmat <- available.packages()  #have to check if Git Hub repository
  deplevels <- c("Imports","Depends")
  #recursive <- T
  #which <- deplevels
  #includebasepkgs <- F

  #create a vector of all needed packages (the vertices)
  all.pkgs <- tools::package_dependencies(pkg, recursive = recursive, which = deplevels, db=bigmat)
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
  if (recursive == T){
    result.df <-do.call(rbind,lapply(all.pkgs, level_pkg))
  } else {

    result.df <-do.call(rbind,lapply(pkg, level_pkg))

  }
  row.names(result.df) <- NULL

  #if necessary delete all connections to base packages
  if ((includebasepkgs == F)) {
    result.df <- subset(result.df, !(result.df[, 1] %in% base.pkgs))
    result.df <- subset(result.df, !(result.df[, 2] %in% base.pkgs))
    all.pkgs <- subset(all.pkgs, !(all.pkgs %in% base.pkgs))
  }

  if(length(outtype) == 1){
    if(outtype == "edgelist"){
      return(result.df[,-3])
    } else if (outtype == "edgelistdetailed"){
      return(result.df)
    } else if (outtype == "allpackages"){
      return(all.pkgs)
    } else if (outtype == "list"){
      return(tools::package_dependencies(pkg, recursive = T , which = deplevels, db=bigmat))
    } else if (outtype == "uniquelist"){
      list <- tools::package_dependencies(pkg, recursive = T , which = deplevels, db=bigmat)
      uniquelist <- list()
      for(i in 1:length(list)){
        everythingelse <- unname(unlist(list[-i]))
        uniquelist[[i]] <- setdiff(list[[i]], everythingelse)
      }
      names(uniquelist) <- pkg
      return(uniquelist)
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
        outlist[[length(outlist)+1]] <- tools::package_dependencies(pkg,
                                  recursive = T , which = deplevels, db=bigmat)
      } else if (outtype[i] == "uniquelist"){
        list <- tools::package_dependencies(pkg, recursive = T , which = deplevels, db=bigmat)
        uniquelist <- list()
        for(i in 1:length(list)){
          everythingelse <- unname(unlist(list[-i]))
          uniquelist[[i]] <- setdiff(list[[i]], everythingelse)
        }
        names(uniquelist) <- pkg
        outlist[[length(outlist)+1]] <- uniquelist
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














