#' @title 'W' paper (working/white) output format
#'
#' @description This is a basic output format appropriate for report drafts,
#' working/white papers, etc. and can also incorporate appendices. No frills for
#' when you just want to focus on the content. This template is based on the work
#' done by Paul Bauer and Camille Landesvatter. See their
#' \href{https://github.com/paulcbauer/Writing_a_reproducable_paper_in_pagedown}{GitHub repository}
#' and the description of \code{pagedown::\link[pagedown]{jss_paged}} for more
#' general information and context.
#'
#' @author Adam Van Iwaarden
#'
#' @param ... Arguments passed to \code{bookdown::\link[bookdown]{html_document2}}
#'   and/or \code{rmarkdown::\link[rmarkdown]{html_document_base}}.
#' @param appendices A list of appendix source file(s) information. The default is
#'   to add a R session information appendix that is rendered after the primary
#'   document has been knitted (post_knit). Pre-rendered appendices (HTML fragments
#'   that use the 'wp_appendix.html' template) and .Rmd that are knitted before
#'   the primary document knitting are also possible.
#' @param template The path to the Pandoc template to convert Markdown to HTML.
#'   The \code{'default'} will use the 'wp_paged.html' template from this package.
#' @param css A character vector of CSS and Sass file paths. If a path
#'   does not contain the \file{.css}, \file{.sass}, or \file{.scss} extension,
#'   it is assumed to be a built-in CSS file. The package \code{'default'}
#'   are \code{c('wp-page.css', 'wp-default.css', 'wp-fonts.css')}.
#' @param csl The path of the Citation Style Language (CSL) file used to format
#'   citations and references (see the
#'   \href{https://pandoc.org/MANUAL.html#citations}{Pandoc documentation}). The
#'   \code{'default'} is the 'american-sociological-association.csl' as used by
#'   Bauer and Landesvatter (see description).
#' @param number_sections Boolean whether to number section headers: if 'TRUE',
#'   figure/table numbers will be of the form 'X.i', where 'X' is the current
#'   first-level number, and 'i' is an incremental number (the i-th figure/table);
#'   if 'FALSE', section figures/tables will be numbered sequentially in the document
#'   from 1, 2, ..., and you cannot cross-reference section headers in this case.
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
#'  \code{\link[pagedown]{jss_paged}}
#'  \code{\link[bookdown]{html_document2}}
#'  \code{\link[rmarkdown]{html_document_base}}
#' @references \url{https://pagedown.rbind.io}
#' @rdname w_paper_paged
#' @import pagedown
#' @importFrom htmltools htmlDependency tagList
#' @importFrom utils packageVersion 
#' @export

w_paper_paged = function(
  ...,
  appendices = list(
    post = list(R = list(file = "r_session_info"))),
  template = "default",
  css = "default",
  csl = "default",
  number_sections = TRUE,
  self_contained = TRUE,
  anchor_sections = FALSE,
  mathjax = NULL,
  pandoc_args = NULL
) {

  if (tolower(template) == "default")
    template <- cfa_paged_res("w_paper", "pandoc", "wp_paged.html")

  if (tolower(css) == "default")
    css <- file.path("rmarkdown/templates/pagedown/w_paper/css",
                     list.files(cfa_paged_res("w_paper", "css")))

  if (tolower(csl) == "default")
    csl <- cfa_paged_res("w_paper", "csl", "american-sociological-association.csl")

  extra_deps <- htmltools::tagList(dep_css(css))

  # Render/Read appendix source(s) and create the `metadata_md` file for pandoc
  if (!is.null(appendices)) {
    ##  Get appendix prefix name(s)
    appdx_prefix <- NULL
    for (nm in names(appendices[])) {
      pfx <- names(appendices[[nm]])
      if (is.null(pfx)) pfx <- "null"
      appdx_prefix <- c(appdx_prefix, pfx)
    }

    if (!is.null(appdx_prefix) & !all(appdx_prefix == "null")) {
      pandoc_args <- c(pandoc_args, "--metadata", 
        paste0("appendix-prefix=", paste(appdx_prefix, collapse=", ")))
    }

    ##  Appendix file(s) passed through arguments will be passed to the pandoc template 
    ##  through --metadata-file. Read/rendered internally, added to a temp .md file used externally
    metadata_md <- tempfile(pattern = "apndx-", fileext = ".md")
    pandoc_args <- c(pandoc_args, "--metadata-file", metadata_md)
    
    ##  Create the markdown meta-data file passed to pandoc and list of text to include
    cat(c("appendix: |", "  "), sep="\n", file = metadata_md)
    
    ##  Create an empty list object to be populated with appendix text (and ordered according to names)
    # apndx_list <- list()

    # if ()
  }

  wp_format = pagedown:::html_format(
    # ...,
    template = template, css = NULL, csl = csl, number_sections = number_sections,
    self_contained = self_contained, anchor_sections = anchor_sections, 
    mathjax = mathjax, theme = NULL, highlight = NULL,
    .pagedjs = TRUE, .pandoc_args = c(
      pagedown:::lua_filters("uri-to-fn.lua", "footnotes.lua", "jss.lua"),
      "--lua-filter", cfa_paged_res("w_paper", "lua", "multiple-bibliographies.lua"),
      if (!is.null(csl)) c("--csl", csl),
      "--metadata", "link-citations=true",
      pandoc_args
    ),
    .dependencies = extra_deps
  )

  opts_wp = list(
    prompt = TRUE, comment = NA, R.options = list(prompt = "R> ", continue = "R+ "),
    fig.align = "center", fig.width = 4.9, fig.height = 3.675,
    class.source = "r-chunk-code", dev = "svg"
  )
  for (i in names(opts_wp)) {
    wp_format$knitr$opts_chunk[[i]] = opts_wp[[i]]
  }

# appendices = list(post = list(R = list(file = "r_session_info", title="R things"), T = list(file="my/test/doc.Rmd", title="Analysis stuff")))
# appendices = list(post = list(`R-` = list(file = "r_session_info"), T = list(file="my/test/doc.Rmd", title="Analysis stuff")))
# appendices = list(post = list(R = list(file = "r_session_info")), pre = list(T = list(file="my/test/doc.Rmd", title="Analysis stuff")))

  nms <- function(n) unlist(appendices[[n]][]) # unlist(lapply(appendices[[n]], function(f) f[["file"]]), use.names = F)

  if (!is.null(appendices[["post"]])) {
    if ("r_session_info" %in% nms("post")) {
      rses <- which(unlist(lapply(appendices[["post"]][], function(f) f[["file"]] == "r_session_info")))
      appendices[["post"]][[rses]][["file"]] <- cfa_paged_res("w_paper", "rmd", "Appendix_R.Rmd")

      if (is.null(appendices[["post"]][[rses]][["title"]])) {
        appendices[["post"]][[rses]][["title"]] <- paste("Appendix", gsub("[[:punct:]]", "", names(rses)))
        appendices[["post"]][[rses]][["subtitle"]] <- "System and Software Information"
      }
    }

    # wp_format$post_knit_orig <- wp_format$post_knit
    wp_format$post_knit = function(
      front_matter, knit_input, runtime, encoding,  # this line are args passed internally - put first and in order!
      postk_apnd = appendices[["post"]], tmp_file = metadata_md) {
        # op(base(...), overlay(...)) # original wp_format$post_knit
        for (i in sequence(length(postk_apnd))) {
          
          to_pandoc <- NULL
          title <- postk_apnd[[i]][["title"]]
          prefix <- names(postk_apnd)[i]
          if (!is.null(title))
            to_pandoc <- c("--metadata", paste0("title=", title))
          if (!is.null(prefix))
            to_pandoc <- c(to_pandoc, "--metadata", paste0("appendix-prefix=", prefix))
        
        #   if (exists("params")) {
        # #     tmp_env <- environment()
        # #     try(if(bindingIsLocked("params", env = tmp_env)) unlockBinding("params", env = tmp_env))  #  required if params is sent in through YAML
        # #     params$prefix <- prefix
        # #   } else {
        #     rm(params);gc()
        #     params <- list(prefix = prefix)
        #   }
        #   params <- list(prefix = prefix)

        # anotherway <- prefix # WORKS! Duh...

          res <- rmarkdown::render(postk_apnd[[i]][["file"]], 
                     output_format = rmarkdown::html_document(
                     template = cfa_paged_res("w_paper", "pandoc", "wp_appendix.html"),
                     number_sections = number_sections,
                     self_contained = self_contained,
                     anchor_sections = anchor_sections,
                     mathjax = mathjax, theme = NULL, highlight = NULL,
                     pandoc_args = to_pandoc), # , params = list(prefix = prefix)
                   output_dir = tempdir()) # , quiet = TRUE, envir = new.env()
          cat(readLines(res), sep = "\n  ", file = tmp_file, append = TRUE)
      }
    }
  }

  
#   cat(readLines(res), sep = "\n  ", file = tmp_file, append = TRUE)


  wp_format
}
