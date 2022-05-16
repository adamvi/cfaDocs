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
#' @import rmarkdown 
#' @importFrom htmltools htmlDependency tagList
#' @export

w_paper_paged = function(
  ...,
  appendices = list(
    post_knit = list(R = list(file = "r_session_info"))),
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
    for (nm in tolower(names(appendices))) {
      pfx <- names(appendices[[nm]])
      if (is.null(pfx)) pfx <- "null"
      appdx_prefix <- c(appdx_prefix, pfx)
    }

    if (!is.null(appdx_prefix) & !all(appdx_prefix == "null")) {
      prefix_md <- tempfile(pattern = "prefix-", fileext = ".md")
      cat(paste0("appendix-prefix: [", paste(appdx_prefix, collapse=", "), "]"),
          sep="\n", file = prefix_md)
      pandoc_args <- c(pandoc_args, "--metadata-file", prefix_md)
    }

    ##  Appendix file(s) passed through arguments will be passed to the pandoc template 
    ##  through --metadata-file. Read/rendered internally, added to a temp .md file used externally
    metadata_md <- tempfile(pattern = "apndx-", fileext = ".md")
    pandoc_args <- c(pandoc_args, "--metadata-file", metadata_md)
    
    ##  Create the markdown meta-data file passed to pandoc and list of text to include
    cat(c("appendix: |", "  "), sep="\n", file = metadata_md)
  }

  wp_format = pagedown:::html_format(
    ...,
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

  ##  Set up output format for rendered, pre_ & post_knit appendices if requested

  if (!is.null(appendices)) {
    nms <- function(n) unlist(appendices[[n]][]) # unlist(lapply(appendices[[n]], function(f) f[["file"]]), use.names = F)

    ##  Pre-knit rendering of appendices:
    if (!is.null(appendices[["pre_knit"]])) {
      wp_format$pre_knit = function(
      input, # internal args passed to pre_knit function - put first and in order!
      # custom args for post-knit rendering of appendices:
      apnd_prek = appendices[["pre_knit"]], metadata_file = metadata_md) {
        for (i in sequence(length(apnd_prek))) {
          to_pandoc <- NULL
          title <- apnd_prek[[i]][["title"]]
          prefix <- names(apnd_prek)[i]
          if (!is.null(title))
            to_pandoc <- c("--metadata", paste0("title=", title))
          if (!is.null(prefix))
            to_pandoc <- c(to_pandoc, "--metadata", paste0("appendix-prefix=", prefix))

          res <- rmarkdown::render(apnd_prek[[i]][["file"]], 
                                   output_format = rmarkdown::html_document(
                                     template = cfa_paged_res("w_paper", "pandoc", "wp_appendix.html"),
                                     number_sections = number_sections,
                                     self_contained = self_contained,
                                     anchor_sections = anchor_sections,
                                     mathjax = mathjax, theme = NULL, highlight = NULL,
                                     pandoc_args = to_pandoc),
                                   output_dir = tempdir())
        }
      }
    }

    wp_format$post_knit = function(
      # internal args passed to post_knit function - put first and in order!
      front_matter, knit_input, runtime, encoding,
      # custom args for post-knit rendering of appendices
      apndcs_arg = appendices, metadata_file = metadata_md) {
        ##  Post-knit rendering of appendices:
        if (!is.null(apndcs_arg[["post_knit"]])) {
          if ("r_session_info" %in% nms("post_knit")) {
            rses <- which(unlist(lapply(apndcs_arg[["post_knit"]][], function(f) f[["file"]] == "r_session_info")))
            apndcs_arg[["post_knit"]][[rses]][["file"]] <- cfa_paged_res("w_paper", "rmd", "Appendix_R.Rmd")

            if (is.null(apndcs_arg[["post_knit"]][[rses]][["title"]])) {
              apndcs_arg[["post_knit"]][[rses]][["title"]] <- paste("Appendix", gsub("[[:punct:]]", "", names(rses)))
              apndcs_arg[["post_knit"]][[rses]][["subtitle"]] <- "System and Software Information"
            }
          }

          for (i in sequence(length(apndcs_arg[["post_knit"]]))) {
            to_pandoc <- NULL
            title <- apndcs_arg[["post_knit"]][[i]][["title"]]
            prefix <- names(apndcs_arg[["post_knit"]])[i]
            if (!is.null(title))
              to_pandoc <- c("--metadata", paste0("title=", title))
            if (!is.null(prefix))
              to_pandoc <- c(to_pandoc, "--metadata", paste0("appendix-prefix=", prefix))

            rmarkdown::render(apndcs_arg[["post_knit"]][[i]][["file"]], 
                              output_format = rmarkdown::html_document(
                                template = cfa_paged_res("w_paper", "pandoc", "wp_appendix.html"),
                                number_sections = number_sections,
                                self_contained = self_contained,
                                anchor_sections = anchor_sections,
                                mathjax = mathjax, theme = NULL, highlight = NULL,
                                pandoc_args = to_pandoc),
                              output_dir = tempdir())
          }
        }

        ##  Create an empty list object to be populated with appendix text (and ordered according to names)
        apndx_list <- list()

        if ("rendered" %in% tolower(names(apndcs_arg))) {
          for (nm in names(apndcs_arg[["rendered"]])) {
            tmp.file <- apndcs_arg[["rendered"]][[nm]][["file"]]
            if (!grepl(".html", tolower(basename(tmp.file))))
              stop("\n\t'rendered' appendices must be a .html file.")
            apndx_list[[nm]] <- readLines(tmp.file)
          }
        }
            
        if ("pre_knit" %in% tolower(names(apndcs_arg))) {
          for (nm in names(apndcs_arg[["pre_knit"]])) {
            tmp.file <- file.path(tempdir(),
                                  gsub(".rmd|.Rmd|.RMD", ".html",
                                  basename(apndcs_arg[["pre_knit"]][[nm]][["file"]])))
            apndx_list[[nm]] <- readLines(tmp.file)
          }
        }

        if ("post_knit" %in% tolower(names(apndcs_arg))) {
          for (nm in names(apndcs_arg[["post_knit"]])) {
            tmp.file <- file.path(tempdir(),
                                  gsub(".rmd|.Rmd|.RMD", ".html",
                                  basename(apndcs_arg[["post_knit"]][[nm]][["file"]])))
            apndx_list[[nm]] <- readLines(tmp.file)
          }
        }

        # order/sort apndx_list by prefix
        apndx_list <- apndx_list[order(names(apndx_list))]

        for (APNDX in names(apndx_list)) {
          cat(apndx_list[[APNDX]], sep = "\n  ", file = metadata_file, append = TRUE)
        }
    }  #  END wp_format$post_knit
  }  #  END `appendices`

  wp_format
}
