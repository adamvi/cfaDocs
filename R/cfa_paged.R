#' @title CFA themed report output format
#' 
#' @description See the description for \code{pagedown::\link[pagedown]{html_paged}}.
#' This is an alternate output format created for the Center for Assessment.
#' 
#' @author Adam Van Iwaarden
#' 
#' @param ... Arguments passed to \code{bookdown::\link[bookdown]{html_document2}}
#'   and/or \code{rmarkdown::\link[rmarkdown]{html_document_base}}.
#' @param template The path to the Pandoc template to convert Markdown to HTML.
#'   The \code{'default'} will use the 'cfa_paged.html' template from this package.
#' @param paged_img The images to be used as CSS variables for the cover, footer,
#'   and other uses. Defaults are 'cfa-front-cover.svg', 'cfa-footer.svg', and
#'   'cfa-logo.svg'. Note that if customized this should be a named character string
#'   to take advantage of default uses. For example: \code{c(`front-cover` =
#'   'path/to/my-front-cover.svg', `footer` = 'path/to/my-footer.svg',
#'   `logo` = 'path/to/my-logo.svg')}.
#' @param css A character vector of CSS and Sass file paths. If a path
#'   does not contain the \file{.css}, \file{.sass}, or \file{.scss} extension,
#'   it is assumed to be a built-in CSS file. The package \code{'default'}
#'   are \code{c('cfa-page.css', 'cfa-default.css', 'cfa-style.css')}.
#' @param csl The path of the Citation Style Language (CSL) file used to format
#'   citations and references (see the
#'   \href{https://pandoc.org/MANUAL.html#citations}{Pandoc documentation}).
#' @param toc Boolean to include a table of contents in the output. Default: TRUE
#' @param toc_depth Depth of headers to include in table of contents. Default: 2
#' @param number_sections Boolean whether to number section headers: if 'TRUE',
#'   figure/table numbers will be of the form 'X.i', where 'X' is the current first-level
#'   number, and 'i' is an incremental number (the i-th figure/table); if 'FALSE',
#'   section figures/tables will be numbered sequentially in the document from 1, 2,
#'   ..., and you cannot cross-reference section headers in this case.
#' @param self_contained Produce a standalone HTML file with no external dependencies,
#'   using data: URIs to incorporate the contents of linked scripts, stylesheets,
#'   images, and videos. Note that even for self contained documents MathJax is
#'   still loaded externally (this is necessary because of its size). Default: TRUE
#' @param anchor_sections to show section anchors when mouse hovers for all headers.
#'   A list can also be passed with 'style' and/or 'depth' to customize the behavior.
#'   See Anchor Sections Customization section., Default: FALSE
#' @param mathjax Include mathjax. Passing \code{'default'} uses an https URL from
#'   a MathJax CDN. The 'local' option uses a local version of MathJax (which is
#'   copied into the output directory). You can pass an alternate URL or pass
#'   \code{NULL} to exclude MathJax entirely. Default: NULL
#' @param pandoc_args Additional command line options to pass to pandoc, Default: NULL
#' @return An R Markdown output format.
#' @seealso
#'  \code{\link[pagedown]{html_paged}}
#'  \code{\link[bookdown]{html_document2}}
#'  \code{\link[rmarkdown]{html_document_base}}
#' @references \url{https://pagedown.rbind.io}
#' @rdname cfa_paged
#' @import pagedown
#' @importFrom rmarkdown includes
#' @importFrom htmltools htmlDependency tagList
#' @importFrom utils packageVersion
#' @export

cfa_paged = function(
  ...,
  template = "default",
  paged_img = "default",
  css = "default",
  csl = NULL,
  toc = TRUE,
  toc_depth = 2L,
  number_sections = FALSE,
  self_contained = TRUE,
  anchor_sections = FALSE,
  mathjax = NULL,
  pandoc_args = NULL
) {

  if (tolower(template) == "default")
    template <- cfa_paged_res("cfa", "pandoc", "cfa_paged.html")

  if (tolower(css) == "default")
    css <- file.path("rmarkdown/templates/pagedown/cfa/css",
                     grep("cfa-", list.files(cfa_paged_res("cfa", "css")), value = TRUE))

  if (all(identical(tolower(paged_img), "default"))) {
    paged_img <- c(
      file.path("rmarkdown/templates/pagedown/cfa/img", 
              c("cfa-front-cover.svg", "cfa-footer.svg")),
      file.path("rmarkdown/shared_resources/img", "cfa-logo.svg"))
    names(paged_img) <- gsub(".svg", "", basename(paged_img))
  } else {
    if (is.null(names(paged_img))) 
      stop("\n\tThe `paged_img` argument must be a named character vector.\n\n")

    if (!all(grepl("^cfa-", names(paged_img))))
      names(paged_img) <- paste0("cfa-", names(paged_img))

    if (!all(names(paged_img) %in% c("cfa-front-cover", "cfa-footer", "cfa-logo")))
      stop("\n\tThe names of `paged_img` argument must be:
            \t\t'front-cover', 'footer' and 'logo' OR
            \t\t'cfa-front-cover', 'cfa-footer' and 'cfa-logo'\n\n")
  }

  extra_deps <- cfa_dependencies(css, paged_img)
  extra_deps <- extra_deps[lengths(extra_deps) != 0]

  pagedown:::html_format(
    ...,
    template = template, css = NULL, toc = toc, toc_depth = toc_depth,
    number_sections = number_sections, self_contained = self_contained, 
    anchor_sections = anchor_sections, mathjax = mathjax, theme = NULL, highlight = NULL,
    includes = rmarkdown::includes(in_header = cfa_paged_res("cfa", "js", "loft.html")), # needs to be passed through includes/in_header to work right.
    .pagedjs = TRUE, .pandoc_args = c(
      pagedown:::lua_filters("uri-to-fn.lua", "loft.lua", "footnotes.lua"), # uri-to-fn.lua must come before footnotes.lua
      if (!is.null(csl)) c("--csl", csl),
      pandoc_args
    ),
    .dependencies = extra_deps
  )
}

##  Internal (private) util functions
dep_css = function(styshts) {
  htmltools::htmlDependency(
    name = "css",
    version = utils::packageVersion("cfaDocs"),
    package = "cfaDocs",
    src = c(file = dirname(styshts)[1]), # can only have one - should all be located in same dir
    stylesheet = basename(styshts),
    all_files = FALSE
  )
}

dep_js = function() {
  htmltools::htmlDependency(
    name = "js",
    version = utils::packageVersion("cfaDocs"),
    package = "cfaDocs",
    src = c(
      file = "rmarkdown/templates/pagedown/cfa/js"
    ),
    script = "cfa-img-vars.js",
    all_files = FALSE
  )
}

dep_img = function(img) {
  htmltools::htmlDependency(
    name = names(img),
    version = utils::packageVersion("cfaDocs"),
    package = "cfaDocs",
    src = c(
      file = dirname(img)
    ),
    attachment = basename(img),
    all_files = FALSE
  )
}

cfa_dependencies = function(CSS, IMG) {
  htmltools::tagList(
    dep_css(CSS),
    dep_js(),
    if (!is.na(IMG["cfa-front-cover"])) dep_img(IMG["cfa-front-cover"]),
    if (!is.na(IMG["cfa-footer"])) dep_img(IMG["cfa-footer"]),
    if (!is.na(IMG["cfa-logo"])) dep_img(IMG["cfa-logo"]),
  )
}
