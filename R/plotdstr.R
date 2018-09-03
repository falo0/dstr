#' Plot Dependency Structure
#'
#' This function plots the dependency structur of one or more packages.
#'
#' The default assumption is that there is an R package in the current working
#' directory and that the dependencies to be analyzed are given in the DESCRIPTION
#' file. Use the parameters ‘githublink’ and/or 'pkg' to alter the package/s
#' to be analyzed.
#'
#' @param githublink A link to a github repository of an R package
#' @param pkg A vector of packages from which we want to know the further
#' dependencies.
#' @param includebasepkgs Whether to include base packages in the analysis.
#' @param includerootpkg Whether to include the root package in the plot.
#' @param recursive show dependencies of dependencies.
#'
#' @note
#' The graph is created by using the Fruchterman-Reingold-Algorithm. A problem
#' for those graphs can be overlapping of the vertex labels. If this is the case
#' the authors suggest to use dstr_data for creating either a network object
#' which layout options can be modified for plotting or an edgelist, which can be used in
#' combination with other packages such 'qgraph' or 'ggnet2'.
#' @import igraph
#' @export
#' @importFrom graphics mtext plot

plotdstr <- function(githublink= NULL, pkg=NULL, includebasepkgs = F, recursive = T,
                     includerootpkg = T){


  writeLines("Loading...")

  bigmat <- available.packages(repos= "https://cloud.r-project.org")
  deplevels <- c("Imports", "Depends")


  #Create the edgelists
  if(includerootpkg){
    # Use either a package on github or in the current working directory
    data <- dstr_data(githublink = githublink, pkg = pkg, recursive = recursive,
                       includebasepkgs = includebasepkgs,
                       outtype = c("edgelist2","all",
                                  "lvl1", "root"))
    # In case the package in the current working directory is used (because
    # neither githublink nor pkg were set), further behavior should be as if
    # a githublink was set.
    # githublink <- "Not NULL"
  } else {
    data <- dstr_data(githublink = githublink, pkg = pkg, recursive = recursive,
                      includebasepkgs = includebasepkgs,
                      outtype = c("edgelist","all",
                                  "lvl1", "root"))
  }

  all_edges <- data[[1]]
  names(all_edges) <- c("start.vertex", "end.vertex")
  firstlvl_vertices <- data[[3]]
  github_pkg <- data[[4]]

  if(includerootpkg){
    all_vertices <- c(github_pkg, data[[2]])
  } else {
    all_vertices <- data[[2]]
  }



  if (nrow(all_edges) == 0) {
    stop(("There are no dependencies"))

  }

  #identify first level dependencies
  firstlvl <- all_edges$start.vertex %in% pkg
  all_edges$firstlevel <- firstlvl
  firstlvl_vertices_cran <- all_edges$end.vertex[which(all_edges$firstlevel ==T)]


  if(includerootpkg){
    uniqueVertices <- unique(c(github_pkg, all_vertices))
  } else {
    uniqueVertices <- unique(all_vertices)
  }


  net <- graph_from_data_frame(d=all_edges, vertices = uniqueVertices, directed = T)


  l <- layout_with_fr(net) #define the network algortihm to the "Fruchterman Reingold"-Algorithm
  #color vertices depending on input, firstlvl or other
  V(net)$vcolor <- ifelse(V(net)$name %in% pkg,"steelblue", ifelse(V(net)$name %in% firstlvl_vertices | V(net)$name %in% firstlvl_vertices_cran, "orange","black"))

  V(net)$label.cex <- 1
  V(net)$label.cex[which(V(net)$name %in% pkg)] <- 1.2

  #if (!is.null(githublink))
  {
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


