#' @title Resolve relative paths
#' @description Find a file path relative to another. 
#'
#' @param path Character. File path from which to find relative path.
#'   Defaults to the current (working) directory
#' @param relation Character. Relationship from path to return. 
#'  Defaults to the path provided.
#' @return Returns an absolute path - the current working dierectory by default.
#' @details Useful in getting the absolute path from relative paths
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  resolvePath(getwd(), "..")
#'  }
#' }
#' @rdname resolvePath
#' @export

resolvePath <- function(path=".", relation=".") {
  tmp.wd <- getwd()
  setwd(path)
  requested.path <- normalizePath(relation)
  setwd(tmp.wd)
  return(requested.path)
}
