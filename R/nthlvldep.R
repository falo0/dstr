#' Get All Deeper Level Dependencies
#'
#' Function takes package names as input and searches for their package
#' dependencies. It returns the dependencies and dependencies of further
#' dependencies. There are various options for the type of the output.It
#' is poosible to select more than one outtype. This results in a list
#' with each element one defined outtype.
#'
#' @param githublink A link to a github repository of an R package.
#' @param pkg Character input of a packagename you want to see the
#' dependencies of. Multiple input packages can be given in vector format.
#' This option is still valid in case of a of a given githublink.
#' @param outtype Possible output types:
#' \itemize{
#' \item edgelist: An edge list, e.g. to be used for network plots.
#' \item edgelist_inclusive: An edge list, e.g. to be used for network plots, including
#' the root package itself when using a githublink.
#' \item all_packages: An overview of all packages that are eventually loaded. No further
#' structure visible.
#' \item network: An igraph network object which can directly be plotted.
#' \item list: More detailed than allPKs, it's a list tha containns all packages per
#' first level dependency.
#' \item list_inclusive: like list but the first level dependencies are not only
#' used for the names of the list elements but also included in the list elements
#' \item unique_list: like "list", just excluding all packages, that are eventually
#' also loaded by another package in firstlvldep. This way you can see which
#' dependencies will be removed completely if you remove a certain first level
#' dependency (a package that you import).
#' \item unique_list_inclusive: like unique_list, but each first level dependency
#' is included in the corresponding list element IF it is a unique dependency.
#' \item tree: Detailed information about which package depends on which, represented
#' in a data frame that is showing a tree structure.
#' \item first_level_packages: The vector of packages used as input. This is a combination
#' when both the githublink and the pkg parameter are set.
#' \item root_package: Whether to include the name of the package of the given
#' github link. Returns "Root Package" when no github link was given.
#' }
#' @param includebasepkgs Whether to include base packages in the analysis or not.
#' @param recursive If TRUE dependencies of dependencies of ... are considered.
#'
#' @examples
#'
#'network_object <- nthlvldep(githublink= "tidyverse/ggplot2", pkg="dplyr",
#' recursive = TRUE, includebasepkgs = FALSE, outtype = "network")
#'
#'#plot(network_object, edge.arrow.size = .1, edge.color="darkgrey",vertex.size = 10,
#'#           vertex.shape = "circle",vertex.frame.color = "white", vertex.label.font= 1,
#'#            vertex.label.color = "black", vertex.color = "white",edge.width = 1.5,
#'#            layout = layout_with_fr)
#'
#'
#' @export
#' @importFrom utils available.packages installed.packages read.csv


nthlvldep <- function(githublink = NULL, pkg = NULL, outtype,
                      includebasepkgs = F, recursive = T){


  # ------- For testing only ---
  #githublink = "Stan125/GREA"
  #githublink = "falo0/dstr"
  #githublink = NULL
  #githublink = "tidyverse/ggplot2"
  #pkg = "NightDay"

  # ----------------------------
  deplevels <- c("Imports","Depends")

  #read the list of base packages
  base_pkgs <- rownames(installed.packages(priority="base"))
  rootPkgName <- NULL

  # Got error message that object 'bigmat' was not found, so I define it here
  # again as a quickfix, will look at it later. Should we not find a good solution in time
  # that updates at least each time the package is used (CRAN is changing all the time),
  # we need to remove sysdata.Rda again.
  bigmat <- utils::available.packages(repos= "https://cloud.r-project.org")

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
  }

  if(includebasepkgs == F){
    pkg <- subset(pkg, !(pkg %in% base_pkgs))
  }

  #create a vector of all needed packages (the vertices)
  frstlvllist <- tools::package_dependencies(pkg, recursive = recursive, which = deplevels, db=bigmat)
  allpkgs <- unique(unname(unlist(frstlvllist)))

  allpkgs <- c(pkg,allpkgs)


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

  create_unique_list <- function(frstlvllist){
    if(length(frstlvllist) >= 1){
      unique_list <- list()
      for(i in 1:length(frstlvllist)){
        everythingelse <- unname(unlist(frstlvllist[-i]))
        unique_list[[i]] <- setdiff(frstlvllist[[i]], everythingelse)
      }
      names(unique_list) <- pkg
      return(unique_list)
    } else {
      return(frstlvllist)
    }
  }

  include_root_in_edgelist <- function(edgelist){
    if(is.null(rootPkgName)){
      return(edgelist)
    } else {
      if (recursive == T){
        firstlevel_frame <- data.frame("start" = rep(rootPkgName,
                        length(github_firstlvl)), "end" = github_firstlvl)

        result_df <- rbind(firstlevel_frame, edgelist)
      } else {
        result_df <- data.frame("start" = rep(rootPkgName, length(github_firstlvl)),
                                "end" = github_firstlvl)
      }
      return(result_df)
    }
  }


    if(length(outtype) == 1){
    if(outtype == "edgelist"){
      return(result_df[,-3])
    } else if (outtype == "edgelist_inclusive"){
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
    } else if (outtype == "all_packages"){
      return(allpkgs)
    } else if (outtype == "list"){
      return(frstlvllist)
    } else if (outtype == "list_inclusive"){
      frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                     function(i) c(names(frstlvllist)[[i]],
                                                   frstlvllist[[i]]))
      names(frstlvllistinclusive) <- names(frstlvllist)
      return(frstlvllistinclusive)
    } else if (outtype == "unique_list"){
      return(create_unique_list(frstlvllist))
    } else if (outtype == "unique_list_inclusive"){
      frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                     function(i) c(names(frstlvllist)[[i]],
                                                   frstlvllist[[i]]))
      names(frstlvllistinclusive) <- names(frstlvllist)
      return(create_unique_list(frstlvllistinclusive))
    } else if (outtype == "tree"){
      treeDF <- edges2tree(result_df[,-3], lvl1deps = pkg)
      # Optionally replace NA by "" so it is nicer to look at. Not sure if we should keep this
      treeDF[is.na(treeDF)] <- ""
      return(treeDF)
    } else if (outtype == "first_level_packages"){
      return(pkg)
    } else if (outtype == "root_package"){
      if(!is.null(rootPkgName)){
        return(rootPkgName)
      } else {
        return("Root Package")
      }
    } else {
      stop("outtype has to be at least one of the following (possibly a vector of
           several): edgelist, edgelist_inclusive, all_packages, list, list_inclusive,
           unique_list, unique_list_inclusive, tree, first_level_packages, root_package")
    }



  } else if (length(outtype) > 1){ # more than one outtype required

    #outtype <- c("edgelist", "allpackges")

    outlist <- list()
    for (i in 1:length(outtype)){
      if(outtype[i] == "edgelist"){
        outlist[[length(outlist)+1]] <- result_df[,-3]
      } else if (outtype[i] == "edgelist_inclusive"){
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
      } else if (outtype[i] == "all_packages"){
        outlist[[length(outlist)+1]] <- allpkgs
      } else if (outtype[i] == "list"){
        outlist[[length(outlist)+1]] <- frstlvllist
      }  else if (outtype[i] == "list_inclusive"){
        frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                       function(i) c(names(frstlvllist)[[i]],
                                                     frstlvllist[[i]]))
        names(frstlvllistinclusive) <- names(frstlvllist)
        outlist[[length(outlist)+1]] <- frstlvllistinclusive
      } else if (outtype[i] == "unique_list"){
        outlist[[length(outlist)+1]] <- create_unique_list(frstlvllist)
      }  else if (outtype[i] == "unique_list_inclusive"){
        frstlvllistinclusive <- lapply(seq_along(frstlvllist),
                                       function(i) c(names(frstlvllist)[[i]],
                                                     frstlvllist[[i]]))
        names(frstlvllistinclusive) <- names(frstlvllist)
        outlist[[length(outlist)+1]] <- create_unique_list(frstlvllistinclusive)
      } else if (outtype[i] == "tree"){
        treeDF <- edges2tree(result_df[,-3], lvl1deps = pkg)
        # Optionally replace NA by "" so it is nicer to look at. Not sure if we should keep this
        treeDF[is.na(treeDF)] <- ""
        outlist[[length(outlist)+1]] <- treeDF
      } else if (outtype[i] == "first_level_packages"){
        outlist[[length(outlist)+1]] <- pkg
      } else if (outtype[i] == "root_package"){
        if(!is.null(rootPkgName)){
          outlist[[length(outlist)+1]] <-rootPkgName
        } else {
          outlist[[length(outlist)+1]] <-"Root Package"
        }
      } else {
        stop("outtype has to be at least one of the following (possibly a vector of
           several): edgelist, edgelist_inclusive, all_packages, list, list_inclusive,
             unique_list, unique_list_inclusive, tree, first_level_packages, root_package")
      }
    }
    #print(outtype)
    #print(outlist)

    names(outlist) <- outtype
    return(outlist)

  } else {
    #outtype has a length of 0
    stop("Your outtype input has length 0")
  }

}


















