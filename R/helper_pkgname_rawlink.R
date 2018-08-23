#' Link To Raw DESCRIPTION, Package Name
#'
#' This function inputs various ways to link to a package or directly the description file on github
#' It outputs the name of the package and a link to the raw DESCRIPTION file.
#' @param githublink A link to the package repository or directly to the description file
#' @export
#' @examples
#' helper_pkgname_rawlink("falo0/dstr")
#' helper_pkgname_rawlink("https://github.com/falo0/dstr")
#' helper_pkgname_rawlink("https://github.com/falo0/dstr/blob/master/DESCRIPTION")

helper_pkgname_rawlink <- function(githublink){

  match <- regmatches(githublink, gregexpr("^(.*.com)",
                                           githublink), invert = T)
  if(length(match[[1]]) == 1){
    descdir <- match[[1]]
  } else {
    descdir <- match[[1]][2]
  }

  descdir <- sub("\\/blob","", descdir)
  pkgname <- descdir
  if (!grepl("/master", descdir)){
    descdir <- paste0(descdir, "/master")
  } else{
    pkgname <- sub("\\/master","", pkgname)
  }
  if (!grepl("/DESCRIPTION", descdir)){
    descdir <- paste0(descdir, "/DESCRIPTION")
  } else{
    pkgname <- sub("\\/DESCRIPTION","", pkgname)
  }

  if(!grepl("^\\/", descdir)){
    # if descdir should not start with "/", add it so it can be attached to the
    # github link below
    descdir <- paste0("/", descdir)
  }

  githublink <- paste0("https://raw.githubusercontent.com", descdir)

  pkgname <- regmatches(pkgname, gregexpr("^(.*\\/)",
                                          pkgname), invert = T)[[1]][2]
  c(pkgname = pkgname, rawlink = githublink)
}


