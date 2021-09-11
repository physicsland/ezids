#' use this function to conveniently load libraries and work smoothly with knitting
#' can add quietly=T option to the require() function
#' the loadPkg function essentially replaced/substituted two functions install.packages() and library() in one step.
#' alternative CRAN example, "http://cran.rstudio.com"
#' Combining install.packages() (if needed) with library() in one, knitr friendly.
#' ELo 2019 GWU DATS
#' @param pkg Name of package
#' @param reposurl CRAN/repo url
#' @return NULL
#' @examples loadPkg("ggplot2", "http://cran.rstudio.com")
#' @export
loadPkg <- function(pkg, reposurl="http://cran.us.r-project.org") {
  if (!require(pkg,character.only=T, quietly =T)) {
    install.packages(pkg,dep=T,repos=reposurl);
    if(!require(pkg,character.only=T)) stop("Package not found")
  }
}

#' unload packages when done
#' ELo 2019 GWU DATS
#' @param pkg Name of package
#' @param character.only character.only = FALSE default
#' @return NULL
#' @examples unloadPkg("ggplot2")
#' @export
unloadPkg <- function(pkg, character.only = FALSE) {
  if(!character.only) { pkg <- as.character(substitute(pkg)) }
  search_item <- paste("package", pkg,sep = ":")
  while(search_item %in% search()) { detach(search_item, unload = TRUE, character.only = TRUE) }
}

