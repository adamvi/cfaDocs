#' @importFrom stats setNames

# Utils adapted from `xaringan`

pkg_resource <- function(...) {
  system.file(
    "rmarkdown", "templates", "xaringan", "resources", ...,
    package = "cfaDocs", mustWork = TRUE
  )
}


xrgn_resource <- function(...) {
  system.file(
    "rmarkdown", "templates", "xaringan", "resources", ...,
    package = "xaringan", mustWork = TRUE
  )
}


list_css <- function() {
  css <- list.files(pkg_resource("css"), "[.]css$", full.names = TRUE)
  stats::setNames(css, gsub(".css$", "", basename(css)))
}


check_builtin_css <- function(theme) {
  valid <- names(list_css())
  if (length(invalid <- setdiff(theme, valid)) == 0) {
    return()
  }
  invalid <- invalid[1]
  maybe <- sort(agrep(invalid, valid, value = TRUE))[1]
  hint <- if (is.na(maybe)) "" else paste0('; did you mean "', maybe, '"?')
  stop(
    '"', invalid, '" is not a valid cfaDocs theme', if (hint != "") hint else ".",
    " Use `cfaDocs:::list_css()` to view all built-in themes.", call. = FALSE
  )
}


copyResources <- function(type="css", resources, ...) {
  file.copy(file.path(pkg_resource(type), resources), xrgn_resource(), overwrite = TRUE)
}
