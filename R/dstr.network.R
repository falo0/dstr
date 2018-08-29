# a wrapper function which creates an igraph network object from input packages.
# The network object can be used for plotting a user defined network graph
dstr.network <- function(githublink = NULL, pkg = NULL,includebasepkgs = F, recursive = T) {

  #create edgelist
  edgelist <- nthlvldep(githublink = githublink, pkg = pkg, outtype="edgelistdetailed", includebasepkgs = includebasepkgs, recursive = recursive)



  network <- igraph::graph_from_data_frame(d=edgelist, directed = T)

  return(network)
}

#mynetwork <- dstr.network(pkg="miniCRAN", githublink = "https://github.com/tidyverse/ggplot2")
#plot(mynetwork)
