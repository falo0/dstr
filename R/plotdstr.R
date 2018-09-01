#' Plot Dependencs Structure
#'
#' This function plots the dependency structur of a package.
#'
#' The default assumption is that there is an R package in the current working
#' directory and that the dependencies to be analyzed are given in the DESCRIPTION
#' file. Use the parameters ‘githublink’ and/or 'pkg' to alter the package/s
#' to be analyzed.
#' @param githublink A link to a github repository of an R package
#' @param pkg A list of packages from which we want to know the further
#' dependencies. This list will be added to the first level dependencies
#' of a given package on github it githublink is set.
#' @param includebasepkgs Whether to include base packages in the analysis.
#' @param includerootpkg Whether to include the root package in the plot.
#' @param recursive Whether you want to look deeper than the second level of dependencies,
#' e.g. get all dependencies of dependencies of dependencies ...
#' @import igraph
#' @export
#' @importFrom graphics mtext plot

plotdstr <- function(githublink= NULL, pkg=NULL, includebasepkgs = F, recursive = T,
                     includerootpkg = T){
  #pkg <- frst
  #pkg <- c("NightDay", "data.table", "gtable")
  #recursive <- T
  #includebasepkgs <- F
  #githublink <- "tidyverse/ggplot2"
  #githublink <- NULL

  writeLines("Loading...")

  bigmat <- available.packages()
  deplevels <- c("Imports", "Depends")


  #Create the edgelists
  if((includerootpkg & !is.null(githublink)) |
     (includerootpkg & is.null(githublink) & is.null(pkg))){
    # Use either a package on github or in the current working directory
    data <- nthlvldep(githublink = githublink, pkg = pkg, recursive = recursive,
                       includebasepkgs = includebasepkgs,
                       outtype = c("edgelist_inclusive","all_packages",
                                  "first_level_packages", "root_package"))
    # In case the package in the current working directory is used (because
    # neither githublink nor pkg were set), further behavior should be as if
    # a githublink was set.
    githublink <- "Not NULL"
  } else {
    data <- nthlvldep(githublink = githublink, pkg = pkg, recursive = recursive,
                      includebasepkgs = includebasepkgs,
                      outtype = c("edgelist","all_packages",
                                  "first_level_packages", "root_package"))
  }

  all_edges <- data[[1]]
  names(all_edges) <- c("start.vertex", "end.vertex")
  firstlvl_vertices <- data[[3]]
  github_pkg <- data[[4]]
  if(includerootpkg & !is.null(githublink)){
    all_vertices <- c(github_pkg, data[[2]])
  } else {
    all_vertices <- data[[2]]
  }

  #inedgel <- unique(c(as.character(all_edges[,1]), as.character(all_edges[,2])))
  #inedgel[!inedgel %in% all_vertices]

  if (nrow(all_edges) == 0) {
    stop(("There are no dependencies"))
    # Feature request: Is it possible to only draw the packages in "pkg"
    # (which are not connected to anything of course) and then give this warning?
    # warning("There are no dependencies")
    # This is probably the behavior the user expects instead of an error message
    # denying to plot anything
  }

  #identify first level dependencies
  firstlvl <- all_edges$start.vertex %in% pkg
  all_edges$firstlevel <- firstlvl

  # Ich glaube daruf geht der Code weiter unten schon ein, bin mir nicht ganz sicher:
  # Es kann auch first level packages geben, die selbst keine depencendies haben
  # Diese sollten auch im netplot erscheinen (die schweben dann halt alleine
  # ohne irgend eine Verbindung herum. Dass keine weiteren Dependencies da sind,
  # ist ja auch eine Information)

  #create network
  # Achtung! Im treeDF kann man gut erkennen, wie es wirklich sein kann, dass
  # z.b. Bei GREA zweimal shiny in den allPkgs ist, weil das halt von verschiedenen
  # packages an verschiedenen Levels geladen wird. K.p. ob in solchen Fällen
  # der netplot anders aussehen müsste. Als Quickfix habe ich erstmal unique
  # genommen
  if(includerootpkg & !is.null(githublink)){
    uniqueVertices <- unique(c(github_pkg, all_vertices)) # <- DAUERLÖSUNG??
  } else {
    uniqueVertices <- unique(all_vertices) # <- DAUERLÖSUNG??
  }


  net <- graph_from_data_frame(d=all_edges, vertices = uniqueVertices, directed = T)


  l <- layout_with_fr(net) * 2 #define the network algortihm to the "Fruchterman Reingold"-Algorithm

  #color vertices depending on input, firstlvl or other
  V(net)$vcolor <- ifelse(V(net)$name %in% pkg,"steelblue", ifelse(V(net)$name %in% firstlvl_vertices, "orange","black"))

  V(net)$label.cex <- 1
  V(net)$label.cex[which(V(net)$name %in% pkg)] <- 1.2

  if (!is.null(githublink)){
    V(net)$label.cex[which(V(net)$name %in% github_pkg)] <- 1.2
    V(net)$vcolor[which(V(net)$name %in% github_pkg)] <- "red"
  }

  #check if there are unidentified packages
  lonely_vertices <- V(net)$name[degree(net, mode="all")==0]
  if (length(lonely_vertices >  0)){
    for (i in  1:length(lonely_vertices)){
      if(!(lonely_vertices[i] %in% bigmat[,1])){
        warning(paste0("Package ",lonely_vertices[i]," can't be found in repository. Are you sure you entered the packagename correctly?"))
      }
    }
  }
  plot(net, edge.arrow.size = .1, edge.color="darkgrey",vertex.size = 10, vertex.shape = "none",
       vertex.frame.color = "white", vertex.label.font= 1, vertex.label.color = V(net)$vcolor,
       edge.width = 1.5 , main = "Dependency Graph",
       frame = T
       , layout = l, vertex.label.cex = V(net)$label.cex)
  mtext(paste("Please enlarge this plot. Number of packages: ",length(all_vertices)), side=1, line=0, adj=1, cex=0.75)
}

#plotdstr(pkg = frst, recursive = T, includebasePkgs = F)
#plotdstr(pkg = c("miniCRAN", "ggplot2"), recursive = T, includebasePkgs = F)
#plotdstr("Stan125/GREA")
