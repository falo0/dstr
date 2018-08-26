#' Convert Edge List To Data Frame With Tree Structure
#'
#' This helper function inputs an edge list and converts it into a data frame
#' where the whole tree structure can be seen.
#' @param edges An edge list as a data frame, first column: starting points, second columns: end points
#' @param lvl1deps The packages that should be at the first level (at the roots) of the tree structure
#' @export
#' @examples
#' edgelist <- data.frame(start = c("shiny", "shiny", "htmltools"),
#'    end = c("jsonlite", "htmltools", "digest"))
#' lvl1deps <- "shiny"
#' edges2tree(edgelist, lvl1deps)

edges2tree <- function(edges, lvl1deps){

  if(length(lvl1deps) > 0){
    edges <- unique(edges)

    # Initialization
    treeDF <- data.frame(NA,lvl1deps)
    tempDFs <- list()

    while(!all(is.na(treeDF[,2]))){
      parents <- as.character(treeDF[,2])
      treeDF <- data.frame(matrix(ncol = 2, nrow = 0))
      for(i in 1:length(parents)){
        parent <- parents[i]
        children <- edges$end[edges$start == parent]
        if(length(children) == 0){
          children <- NA
          #print("hier")
        } else if (is.na(children[1])){
          children <- NA
          #print("unten")
        }
        newDF <- data.frame(rep(parent, length(children)),children)
        treeDF <- rbind(treeDF, newDF)
      }
      # Convert all factors to characters because the factors lead to problems
      # at several points:
      treeDF[] <- lapply(treeDF, as.character)
      tempDFs[[length(tempDFs)+1]] <- treeDF
    }
    # remove last tempDF because it contains only NAs in the second column
    # we needed that to know that there are no deeper level dependencies
    # but we don't want to include it in the final data frame.
    tempDFs <- tempDFs[-length(tempDFs)]

    # merge the tempDFs into one treeDF
    # Dimensions of final treeDF
    n_terminalnodes <- nrow(tempDFs[[length(tempDFs)]])
    n_levels <- length(tempDFs)

    # Merging the data frames
    #Initializing
    old <- tempDFs[[1]]

    for(j in 1:(n_levels-1)){
      #j = 1
      new <- tempDFs[[j+1]]
      #treeDF[1:nrow(old), 1:ncol(old)] <- old
      #treeDF

      i = 1
      while(i <= nrow(new)){
        #print(i)
        #i=25
        parent <- as.character(old[i,1+j])
        if(!is.na(any(new[,1] == parent))){
          if(any(new[,1] == parent)){
            children <- new[,2][new[,1] == parent & !is.na(new[,1])]
            children <- unique(children)
          } else {
            children <- NA
          }
        } else {
          children <- NA
        }
        # Insert the currently observed row directly under itself, so that it is copied
        # as many times as there are children
        if(length(children) > 1){
          for(c in 1:(length(children)-1)){
            if(i < nrow(old)){
              old <- rbind(old[1:i,], old[i,], old[(i+1):nrow(old),])
            } else {
              # if we are in the last row of old
              old <- rbind(old[1:i,], old[i,])
            }
          }
          # manually increase i so that no steps are taken more times than they should
          i = i + length(children)-1
        }

        #old
        i = i + 1

      }
      rownames(old) <- NULL
      #nrow(old)
      #nrow(new)

      treeDF <- old
      treeDF[,2+j] <- new[,2]
      #treeDF
      old <- treeDF
    }


    treeDF <- old
    colnames(treeDF) <- sapply(1:n_levels, function(i){paste0("lvl", i)})
    rownames(treeDF) <- NULL
    return(treeDF)
  } else {
    return(data.frame())
  }
}

#edges <- read.csv("tests/testthat/test_files/GREA_edges.csv")
#lvl1deps <- c("shiny", "miniUI", "rstudioapi", "rio", "R.matlab", "DT")

#edges <- read.csv("tests/testthat/test_files/ggplot2_edges.csv")
#lvl1deps <- read.csv("tests/testthat/test_files/ggplot2_lvl1deps.csv")

#treeDF <- edges2tree(edges, lvl1deps)
#treeDFpretty <- treeDF %>% replace(is.na(.), "")
#View(treeDFpretty)

#write.csv(edgelist, "simple_edges.csv", row.names = F)
