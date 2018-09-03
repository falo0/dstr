#' Get Various Aspects Of The Dependency Structure In The Form Of Vectors, Lists And Data.Frames
#'
#' This function returns certain information about the dependency structure of
#' the package/s to be analyzed in the form of vectors, lists and data.frames.
#' The different aspects of the dependency structure are provided by 11 different
#' output types (set by the parameter 'outtypes', see description below).
#' The default assumption is that there is
#' an R package in the current working directory and that the dependencies to be
#' analyzed are given in the DESCRIPTION file. Use the parameters ‘githublink’
#' and/or 'pkg' to alter the package/s to be analyzed.
#' @param githublink A link to a github repository of an R package.
#' @param pkg Character vector of CRAN package name/s you want to see the
#' dependencies of. In the case that githublink is also set, the github package
#' is considered as the root package and the packages provided
#' by the pkg parameter are considered to be first level packages, e.g. on
#' the same level as the packages in the DESCRIPTION file of the github package.
#' This is to help answer the question "How would the dependency structure change
#' if the package on github would also depend on a few more packages (provided by
#' the pkg parameter)?".
#' @param outtype Possible output types:
#'
#' Key data about the dependency structure:
#' \itemize{
#' \item 'root': The name of the package/s that the analysis is about (level 0
#' in the dependency tree)
#' \item 'lvl1': The dependencies that are given under "Imports:" or "Depends:" in
#' the DESCRIPTION file of the root packge/s (level 1 in the dependency tree).
#' They are also referred to as first level depdencies or first level packages.
#' Dependencies of dependencies of ... (deeper level dependencies), which are
#' not in the DESCRIPTION file, are NOT included.
#' \item 'all': An overview of all packages that are eventually loaded (all levels).
#' No further structure visible.
#' \item 'tree': Detailed information about which package depends on which, represented
#' in a data frame that is showing a tree structure.
#' }
#' Dependencies per first level dependency:
#' \itemize{
#' \item 'list': More detailed than 'all', it's a list that containns all packages per
#' first level dependency.
#' \item 'list2': like 'list' but the first level dependencies are not only
#' used for the names of the list elements but also included in the list elements
#' \item 'unique': like 'list', just excluding all packages, that are eventually
#' also loaded by another first level package. This way you can see which
#' dependencies will be removed completely if you remove a certain first level
#' dependency (a package that you import).
#' \item 'unique2': like 'unique', but each first level dependency
#' is included in the corresponding list element IF it is a unique dependency.
#' E.g. If a certain first level dependency is a deeper level dependency from
#' another first level dependency, the corresponding list entry will be
#' character(0). This means that if you remove that certain first level dependency
#' from your Imports/Depends in the DESCRIPTION file, you actually won't remove
#' it from the whole dependency structure because another first level package
#' still depends on it.
#' }
#' For plotting and network analysis:
#' \itemize{
#' \item edgelist: An edge list, e.g. to be used for network plots, including
#' only level 1 and deeper level dependencies, not the root package/s.
#' \item edgelist2: An edge list, e.g. to be used for network plots, including
#' the root package/s.
#' \item network: An igraph network object which can directly be plotted.
#' }
#'
#' @param includebasepkgs Whether to include base packages in the analysis or not.
#' @param recursive If FALSE dependencies of dependencies of ... are not considered.
#'
#' @examples
#' # Using a package in the local working directory
#' # setwd("path/to/package")
#' # dstr_data(outtype = c("all", "tree"))
#'
#' # Using a package on github
#' network_object <- dstr_data(githublink= "tidyverse/ggplot2", pkg="dplyr",
#'  recursive = TRUE, includebasepkgs = FALSE, outtype = "network")
#'
#' # needs package igraph attached:
#' # plot(network_object, edge.arrow.size = .1, edge.color="darkgrey",vertex.size = 10,
#' #           vertex.shape = "circle",vertex.frame.color = "white", vertex.label.font= 1,
#' #            vertex.label.color = "black", vertex.color = "white",edge.width = 1.5,
#' #            layout = layout_with_fr)
#'
#'
#' @export
#' @importFrom utils available.packages installed.packages read.csv


dstr_data <- function(githublink = NULL, pkg = NULL, outtype,
                      includebasepkgs = F, recursive = T){

  deplevels <- c("Imports","Depends")

  #read the list of base packages
  base_pkgs <- rownames(installed.packages(priority="base"))
  rootPkgName <- NULL


  bigmat <- utils::available.packages(repos= "https://cloud.r-project.org")

  onlyPkg <- F
  if (is.null(githublink) & is.null(pkg)){
    # Neither githublink nor localdir were set
    # Assumption: The current working directory is the directory of an R package
    res <- firstlvldep(includeRootPkg = T)
    rootPkgName <- unname(res[[1]])
    github_firstlvl <- res[[2]]
    pkg <- res[[2]]

  } else if (!is.null(githublink) & is.null(pkg)){
    # only a github link and no vector of packages was given
    res <- firstlvldep(githublink, includeRootPkg = T)
    rootPkgName <- unname(res[[1]])
    github_firstlvl <- res[[2]]
    pkg <- res[[2]]
  } else if (!is.null(githublink) & !is.null(pkg)){
    # both a githublink and a vector of packges where given, e.g. to test
    # what would happen if a certain package would also import the given vector
    # of packages
    res <- firstlvldep(githublink, includeRootPkg = T)
    rootPkgName <- unname(res[[1]])
    github_firstlvl <- res[[2]]
    pkg <- unique(c(res[[2]], pkg))
  } else {
    # only pkg is set. In all other cases, pkg was seen as level 1 packages
    # (on the same level as the dependencies in the description file of a package)
    # now pkg has to be seen as the root packages
    onlyPkg <- T
    rootPkgName <- pkg
    pkg <- unique(unlist(tools::package_dependencies(pkg, recursive = F, which = deplevels, db=bigmat)))
    #github_firstlvl <- pkg
  }

  if(includebasepkgs == F){
    pkg <- subset(pkg, !(pkg %in% base_pkgs))
  }

  #create a vector of all needed packages (the vertices)
  frstlvllist <- tools::package_dependencies(pkg, recursive = recursive, which = deplevels, db=bigmat)
  allpkgs <- unique(c(pkg, unname(unlist(frstlvllist))))


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
  if(length(allpkgs > 0)){
    if (recursive == T){
      result_df <-do.call(rbind,lapply(allpkgs, level_pkg))
    } else {

      result_df <-do.call(rbind,lapply(pkg, level_pkg))
    }
  } else {
    # There are no dependencies at all, return an empty data frame (and not NULL)
    result_df <- data.frame("start" = character(),"end" = character(),
                            "dependency" = character())
  }
  row.names(result_df) <- NULL

  remove_base_pkgs <- function(){
  #if necessary delete all connections to base packages
    if ((includebasepkgs == F)) {
      result_df <<- subset(result_df, !(result_df[, 1] %in% base_pkgs))
      result_df <<- subset(result_df, !(result_df[, 2] %in% base_pkgs))
      allpkgs <<- subset(allpkgs, !(allpkgs %in% base_pkgs))

      frstlvllist <<- lapply(frstlvllist,
                            function(x){subset(x, !(x %in% base_pkgs))})
    }
  }
  remove_base_pkgs()

  create_unique <- function(frstlvllist){
    if(length(frstlvllist) >= 1){
      unique <- list()
      for(i in 1:length(frstlvllist)){
        everythingelse <- unname(unlist(frstlvllist[-i]))
        unique[[i]] <- setdiff(frstlvllist[[i]], everythingelse)
      }
      names(unique) <- pkg
      return(unique)
    } else {
      return(frstlvllist)
    }
  }

  include_root_in_edgelist <- function(edgelist){
    #edgelist <- result_df[,-3]

    if(is.null(rootPkgName)){
      return(edgelist)
    }

    if (onlyPkg){
      # only the pkg parameter is set
      result_df <- data.frame(matrix(ncol = 2, nrow = 0))
      names(result_df) <- c("start", "end")
      for(i in 1:length(rootPkgName)){

        frstlvl <- unlist(tools::package_dependencies(rootPkgName[i],
                                                      recursive = F, which = deplevels, db=bigmat))
        firstlevel_frame <- data.frame("start" = rep(rootPkgName[i],
                                                     length(frstlvl)), "end" = frstlvl)

        result_df <- rbind(result_df, firstlevel_frame)
      }

      if(recursive == T){
        result_df <- rbind(result_df, edgelist)
      }
      row.names(result_df) <- NULL

    } else {

      if (recursive == T){
        firstlevel_frame <- data.frame("start" = rep(rootPkgName,
                        length(github_firstlvl)), "end" = github_firstlvl)

        result_df <- rbind(firstlevel_frame, edgelist)
      } else {
        result_df <- data.frame("start" = rep(rootPkgName, length(github_firstlvl)),
                                "end" = github_firstlvl)
      }

    }
    return(result_df)
  }


    if(length(outtype) == 1){
    if(outtype == "edgelist"){
      return(result_df[,-3])
    } else if (outtype == "edgelist2"){
      result_df <- include_root_in_edgelist(result_df[,-3])
      remove_base_pkgs()
      return(result_df)
    } else if (outtype == "network"){
      result_df <- include_root_in_edgelist(result_df[,-3])
      network <- igraph::graph_from_data_frame(d=result_df, directed = T)
      if(length(V(network)) == 0){
        warning("Your network object has no vertices. There are no dependencies.")
      }
      return(network)
    } else if (outtype == "all"){
      return(allpkgs)
    } else if (outtype == "list"){
      return(frstlvllist)
    } else if (outtype == "list2"){
      frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                     function(i) c(names(frstlvllist)[[i]],
                                                   frstlvllist[[i]]))
      names(frstlvllistinclusive) <- names(frstlvllist)
      return(frstlvllistinclusive)
    } else if (outtype == "unique"){
      return(create_unique(frstlvllist))
    } else if (outtype == "unique2"){
      frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                     function(i) c(names(frstlvllist)[[i]],
                                                   frstlvllist[[i]]))
      names(frstlvllistinclusive) <- names(frstlvllist)
      return(create_unique(frstlvllistinclusive))
    } else if (outtype == "tree"){
      treeDF <- edges2tree(result_df[,-3], lvl1deps = pkg)
      treeDF[is.na(treeDF)] <- ""
      return(treeDF)
    } else if (outtype == "lvl1"){
      return(pkg)
    } else if (outtype == "root"){
      if(!is.null(rootPkgName)){
        return(rootPkgName)
      } else {
        return("Root Package")
      }
    } else {
      stop("outtype has to be at least one of the following (possibly a vector of
           several): edgelist, edgelist2, all, list, list2,
           unique, unique2, tree, network, lvl1, root")
    }



  } else if (length(outtype) > 1){ # more than one outtype required

    #outtype <- c("edgelist", "allpackges")

    outlist <- list()
    for (i in 1:length(outtype)){
      if(outtype[i] == "edgelist"){
        outlist[[length(outlist)+1]] <- result_df[,-3]
      } else if (outtype[i] == "edgelist2"){
        result_df <- include_root_in_edgelist(result_df[,-3])
        remove_base_pkgs()
        outlist[[length(outlist)+1]] <- result_df
      } else if(outtype[i] == "network"){
        result_df <- include_root_in_edgelist(result_df[,-3])
        network <- igraph::graph_from_data_frame(d=result_df, directed = T)
        if(length(V(network)) == 0){
          warning("Your network object has no vertices. There are no dependencies.")
        }
        outlist[[length(outlist)+1]] <- network
      } else if (outtype[i] == "all"){
        outlist[[length(outlist)+1]] <- allpkgs
      } else if (outtype[i] == "list"){
        outlist[[length(outlist)+1]] <- frstlvllist
      }  else if (outtype[i] == "list2"){
        frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                       function(i) c(names(frstlvllist)[[i]],
                                                     frstlvllist[[i]]))
        names(frstlvllistinclusive) <- names(frstlvllist)
        outlist[[length(outlist)+1]] <- frstlvllistinclusive
      } else if (outtype[i] == "unique"){
        outlist[[length(outlist)+1]] <- create_unique(frstlvllist)
      }  else if (outtype[i] == "unique2"){
        frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                       function(i) c(names(frstlvllist)[[i]],
                                                     frstlvllist[[i]]))
        names(frstlvllistinclusive) <- names(frstlvllist)
        outlist[[length(outlist)+1]] <- create_unique(frstlvllistinclusive)
      } else if (outtype[i] == "tree"){
        treeDF <- edges2tree(result_df[,-3], lvl1deps = pkg)
        # Optionally replace NA by "" so it is nicer to look at. Not sure if we should keep this
        treeDF[is.na(treeDF)] <- ""
        outlist[[length(outlist)+1]] <- treeDF
      } else if (outtype[i] == "lvl1"){
        outlist[[length(outlist)+1]] <- pkg
      } else if (outtype[i] == "root"){
        if(!is.null(rootPkgName)){
          outlist[[length(outlist)+1]] <-rootPkgName
        } else {
          outlist[[length(outlist)+1]] <-"Root Package"
        }
      } else {
        stop("outtype has to be at least one of the following (possibly a vector of
           several): edgelist, edgelist2, all, list, list2,
             unique, unique2, tree, network, lvl1, root")
      }
    }


    names(outlist) <- outtype
    return(outlist)

  } else {
    #outtype has a length of 0
    stop("Your outtype input has length 0")
  }

}


















