#' @title Xaringan output format for the Center for Assessment
#'
#' @description Render xaringan/remark.js presentation slides using custom
#' templates and themes. This output format produces an HTML file that contains
#' the Markdown source (knitted from R Markdown) and JavaScript code to render
#' slides.
#'
#' @param cfa_theme Name of CSS based theme for slides (length 1). Alternatively
#'  \code{NULL} with \code{css} argument specified Default: 'cfa-a'
#' @param theme_copy Boolean - should the theme assets be copied to users
#'  xaringan built-in themes, Default: TRUE
#' @param include features to include, such as custom CfA footer and a preset
#'  list of xaringanExtra add-ins. The available elements currently include
#'  \code{footer} which can be the default style or the path to a custom footer
#'  code file, \code{footer_text} is text to display in the default footer
#'  (default text is the Center's web page), \code{xaringanExtra} is the path to
#'  a Rmd file that creates the dependency links for the default used functions,
#'  which include \code{use_logo} and any others included in the \code{xExtra_use}
#'  element (default functions are \code{c("share_again", "slide_tone", "tile_view", "clipboard"))}
#' @param css Optional file paths to css assets (\code{cfa_theme} must be NULL
#'  if used). Users can also provide a mix of css from multiple themes using
#'  a specific file path convention, e.g. \code{c("**/cfa-a.css", "**/cfa-b-fonts.css")}.
#'  Default: NULL
#' @param extras A named list of extra resources to be copied to the \code{lib_dir}, e.g.
#'  \code{list(fig = "cfa_assets/fig")} (or in Rmd YAML as
#'  \code{extras: !expr list(fig = 'cfa_assets/fig')} for figures placed in
#'  Rmd directly with <img> tag). Default: list()
#' @param nature List fed to the \code{nature} argument in
#'  \code{\link[xaringan]{moon_reader}}.
#' @param lib_dir Directory name of external/custom resources to be copied into
#'  (e.g. xaringanExtra js/css dependencies). Default: 'libs'
#' @param asset_dir Directory name of custom resource location. Assumes
#'  subdirectories with names "css", "img", or as given in \code{extras}.
#'  Default: 'cfa_assets'
#' @param ... Additional arguments passed to \code{\link[xaringan]{moon_reader}} 
#'  and \code{\link[rmarkdown]{render}}
#' @return HTML slide presentation.
#' @details Slides are formatted with any combination of CfA based themes
#'
#' @seealso 
#'  \code{\link[htmltools]{htmlDependency}}, \code{\link[htmltools]{copyDependencyToDir}}
#'  \code{\link[rmarkdown]{render}}, \code{\link[rmarkdown]{includes}}
#'  \code{\link[R.utils]{copyDirectory}}
#'  \code{\link[xaringan]{moon_reader}}
#' @references \url{https://github.com/yihui/xaringan/wiki}
#' @rdname cfaXaringan
#' @author Adam Van Iwaarden
#' @keywords documentation presentations
#' @importFrom htmltools htmlDependency copyDependencyToDir
#' @importFrom rmarkdown render includes
#' @importFrom R.utils copyDirectory
#' @importFrom xaringan moon_reader
#' @export
cfaXaringan <- function(
  cfa_theme = "cfa-a",
  theme_copy = TRUE,
  include = list(
    footer = "default",
    xaringanExtra = "default",
    xExtra_use = c("share_again", "slide_tone", "tile_view", "clipboard")),
  css = NULL,
  extras = list(),
  nature = list(
      ratio = "16:9",
      highlightStyle = "github",
      highlightLines = TRUE,
      countIncrementalSlides = FALSE),
  lib_dir = "libs",
  asset_dir = "cfa_assets",
  ...
  ) {
  # check arguments
  if (!is.null(cfa_theme) & !is.null(css)) stop("Specify either `cfa_theme` or `css`, but not both.")
  # need to remove old 'libs' directory or xaringanExtra paths get messed up.
  if (dir.exists(lib_dir)) unlink(lib_dir, recursive = TRUE)

  if (!is.null(cfa_theme)) {
    # Check css
    css_dir <- pkg_resource("css")
    theme.css <- grep(paste(cfa_theme, collapse = "|"), list.files(css_dir), value = TRUE)

    if (theme_copy) {
      copyResources(resources=theme.css)
      theme.css <- gsub("[.](?:sa|sc|c)ss$", "", theme.css)
    } else {
      options(htmltools.dir.version = FALSE)
      theme_css <- if (length(theme.css)) {
        if (is.null(check_builtin_css(cfa_theme))) {
          htmltools::htmlDependency(
            "css", "0.0.1", css_dir,
            stylesheet = theme.css,
            all_files = FALSE
          )
        }
      }
      if (!dir.exists(new_dir <- file.path(lib_dir, asset_dir, "css"))) 
        dir.create(new_dir, recursive = TRUE)
      # assign to css.cp to suppress output.
      css.cp <- htmltools::copyDependencyToDir(theme_css, file.path(lib_dir, asset_dir), mustWork = TRUE)
      theme.css <- file.path(new_dir, theme.css)
    }
  } else {
    # allow user to mix and match cfa theme assets
    options(htmltools.dir.version = FALSE)
    theme.css <- gsub("^\\*\\*", pkg_resource("css"), css)
    theme_css <- htmltools::htmlDependency(
      "css", "0.0.1", dirname(theme.css),
      stylesheet = basename(theme.css),
      all_files = FALSE
    )
    if (!dir.exists(new_dir <- file.path(lib_dir, asset_dir, "css")))
      dir.create(new_dir, recursive = TRUE)
    css.cp <- htmltools::copyDependencyToDir(theme_css, file.path(lib_dir, asset_dir), mustWork = TRUE)
    theme.css <- file.path(new_dir, basename(theme.css))
  }

  #  Things like figures placed directly with <img>, assets without a http url (local only), etc.
  if (length(extras)) {
    for (asset in seq_len(length(extras))) {
      tmp_dir <- extras[[asset]]
      if (!dir.exists(new_dir <- file.path(lib_dir, asset_dir, names(extras)[asset])))
        dir.create(new_dir, recursive = TRUE)
      tmp.cp <- file.copy(list.files(tmp_dir, full.names = TRUE), new_dir, overwrite = TRUE)
    }
  }

  # Create lib_dir if it has not been above (needed for in_header.html at min)
  if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE)

  if (length(include)) {
    if (length(include[["footer"]])) {
      if (include[["footer"]] == "default") {
        foot_md <- makeFooter(ft.text = include[["footer_text"]], lib = lib_dir)
      } else {
        foot_md <- makeFooter(ft.file = include[["footer"]],
                              ft.text = include[["footer_text"]], lib = lib_dir)
      }
    }
    if (length(include[["xaringanExtra"]])) {
      if (include[["xaringanExtra"]] == "default")
        include[["xaringanExtra"]] <- pkg_resource("rmd/cfa-xaringanExtra.Rmd")
      if (!dir.exists(tmp_libdir <- file.path(dirname(include[["xaringanExtra"]]), lib_dir)))
        dir.create(tmp_libdir, recursive = TRUE)
      head_html <- file.path(lib_dir, "in_header.html")
      params <- list(xExtra_use = include[["xExtra_use"]])
      if (!is.null(cfa_theme)) {
        if (cfa_theme == "cfa-a") {
          params[["width"]] <- "150px"
          params[["height"]] <- "100px"
          params[["top"]] <- "1em"
          params[["right"]] <- "1.25em"
          params[["img"]] <- "https://raw.githubusercontent.com/CenterForAssessment/cfaDocs/main/inst/rmarkdown/templates/xaringan/resources/img/nciea-logo-long.svg"
        }
        if (cfa_theme != "cfa-b") {
          params[["width"]] <- "86px"
          params[["height"]] <- "100px"
          params[["top"]] <- "0.5em"
          params[["right"]] <- "0.5em"
          params[["img"]] <- "https://raw.githubusercontent.com/CenterForAssessment/cfaDocs/main/inst/rmarkdown/templates/xaringan/resources/img/CFA_Logo.svg"
        }
      } else { # boilerplate for now.
        params[["width"]] <- "86px"
        params[["height"]] <- "100px"
        params[["top"]] <- "0.5em"
        params[["right"]] <- "0.5em"
        params[["img"]] <- "https://raw.githubusercontent.com/CenterForAssessment/cfaDocs/main/inst/rmarkdown/templates/xaringan/resources/img/CFA_Logo.svg"
      }

      tmp.xE <- rmarkdown::render(include[["xaringanExtra"]], output_file = head_html)
      R.utils::copyDirectory(tmp_libdir, lib_dir)
      unlink(tmp_libdir, recursive = TRUE)
      tmp_html <- readLines(head_html)
      tmp_html <- gsub("<script src=\"", paste0("<script src=\"", lib_dir, "/"), tmp_html)
      tmp_html <- gsub("<link href=\"", paste0("<link href=\"", lib_dir, "/"), tmp_html)
      tmp_html <- c("<link rel=\"icon\" type=\"image/png\" href=\"https://centerforassessment.github.io/assets/favicon.png\" sizes=\"16x16\">", tmp_html)
      writeLines(tmp_html, head_html)
    }
  }

  if (!exists("foot_md")) foot_md <- NULL

  if (!exists("head_html")) {
    head_html <- file.path(lib_dir, "in_header.html")
    writeLines("<link rel=\"icon\" type=\"image/png\" href=\"https://centerforassessment.github.io/assets/favicon.png\" sizes=\"16x16\">", head_html)
  }

  ##  template
  xaringan::moon_reader(
    css = theme.css,
    nature = nature,
    lib_dir = lib_dir,
    includes = rmarkdown::includes(in_header = head_html, before_body = foot_md),
    ...
  )
}
