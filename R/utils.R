#' @importFrom stats setNames

# Utils adapted from `xaringan`

pkg_resource <- function(...) {
  system.file(
    "rmarkdown", "templates", "xaringan", "resources", ...,
    package = "cfaDocs", mustWork = TRUE)
}

xrgn_resource <- function(...) {
  system.file(
    "rmarkdown", "templates", "xaringan", "resources", ...,
    package = "xaringan", mustWork = TRUE)
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
    " Use `cfaDocs:::list_css()` to view all built-in themes.", call. = FALSE)
}


copyResources <- function(type="css", resources, ...) {
  file.copy(file.path(pkg_resource(type), resources), xrgn_resource(), overwrite = TRUE)
}

makeFooter <- function(ft.file=pkg_resource("rmd/cfa-footer.md"), ft.text=NULL, lib) {
  tmp_foot <- readLines(ft.file)
  if (is.null(ft.text)) {
    ft.text <- "<a class=\"footer\" href=\"http://www.nciea.org\">     www.nciea.org</a>"
  } else {
    if (!grepl("^<a class=\"footer\"", ft.text))
      ft.text <- paste0("<a class=\"footer\">", ft.text, "</a>")
  }
  tmp_foot <- gsub("TMP_FOOTER", ft.text, tmp_foot)
  ft.file <- file.path(lib, "footer.md")
  writeLines(c("layout: true", "", tmp_foot, "", "---", ""), ft.file)
  ft.file
}

makeOctoCat <- function(
  oc.file = pkg_resource("rmd/cfa-octocat-corner.md"),
  link = "https://github.com/CenterForAssessment") {
    tmp_octcat <- readLines(oc.file, warn=FALSE)
    tmp_octcat <- sub("TMP_LINK", link, tmp_octcat)
    tmp_octcat
}

# Given the path to a file, return a file:// URL.
# Adapted slightly from webshot2 utils
file_url <- function(filename) {
  if (.Platform$OS.type == "windows") {
    paste0("file://", normalizePath(filename, mustWork = TRUE))
  } else {
    enc2utf8(paste0("file:///", normalizePath(filename, winslash = "/", mustWork = TRUE)))
  }
}
