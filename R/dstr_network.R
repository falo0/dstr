
#' Create a dependency network object.
#'
#'\code{dstr_network()} returns an igraph network object, which can be used for
#' creating individual dependency graphs.
#'
#'@param githublink Link of type character to a github repository of a R package.
#'
#'@param pkg Character input of a packagename you want to plot the dependencies for.
#' Multiple input packages can be given in vector format.
#'
#'@param includebasepkgs If TRUE all dependencies to base R packages are included
#' in the output. Take this option with caution. The output easily gets very chaotic.
#'
#'@param recursive If TRUE the network graph will show dependencies of dependencies.
#'
#'@return Output will be an igraph network object which can be manipulated by standard
#'igraph rules.
#'
#'@examples
#'
#'#network_object <- dstr_network(githublink = "falo0/dstr", pkg = "ggplot2")
#
#'#plot.igraph(network_object, edge.arrow.size = .1, edge.color="darkgrey",vertex.size = 10,
#'#           vertex.shape = "circle",vertex.frame.color = "white", vertex.label.font= 1,
#'#            vertex.label.color = "black", vertex.color = "white",edge.width = 1.5,
#'#            layout = layout_with_fr)
#'
#'@export


dstr_network <- function(githublink = NULL, pkg = NULL,includebasepkgs = F, recursive = T) {

  #create edgelist
  edgelist <- nthlvldep(githublink = githublink, pkg = pkg, outtype="edgelistdetailed", includebasepkgs = includebasepkgs, recursive = recursive)



  network <- igraph::graph_from_data_frame(d=edgelist, directed = T)

  return(network)
}

#mynetwork <- dstr.network(pkg="miniCRAN", githublink = "https://github.com/tidyverse/ggplot2")
#plot(mynetwork)
