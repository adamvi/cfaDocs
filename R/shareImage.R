#' @title Screenshot Your Title Slide for Share Image
#' @description Takes a screenshot of your title slide for sharing on Twitter
#' (and other social media sites).

#' @param slides_rmd File name (potentially with path) of slide .Rmd
#' @param width Output image width (in pixels/px). Default: 600 * (191/100), must be round
#' @param height Output image height (in pixels/px). Default: 600, must be round
#' @param ratio Character representation of the \code{\link[xaringan]{moon_reader}} 
#'  ratio list element provided in the `nature` argument.
#' @param path_image Path to new share image. Default: NULL will create a .png
#'  file with the same path/name as `slides_rmd` with '_share_image.png' appended.
#' 
#' @return file name/path of sharable png image.
#' @details Uses \code{\link[webshot2]{rmdshot}} to re-render slides in the desired
#'  output ratio and creates an image of the Title page. Adapted from Garrick Aden-Buie
#'  blog (see references).
#' 
#' @references \url{https://www.garrickadenbuie.com/blog/sharing-xaringan-slides/#the-perfect-share-image-ratio}
#' @seealso 
#'  \code{\link[rmarkdown]{yaml_front_matter}}
#'  \code{\link[yaml]{as.yaml}}
#'  \code{\link[webshot2]{rmdshot}}
#'  \code{\link[xaringan]{moon_reader}}
#'
#' @rdname shareImage
#' @export 
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom yaml as.yaml
#' @importFrom webshot2 rmdshot
shareImage <- function(
    slides_rmd,
    width = 600*(191/100),
    height = 600,
    ratio = "191:100",
    path_image = NULL) 
  {
    if (!requireNamespace("webshot2", quietly = TRUE)) {
      stop(
        "`webshot2` is required: ",
        'remotes::install_github("rstudio/webshot2")')
    }

    width <- round(width)
    height <- round(height)

    if (is.null(path_image)) {
      out.dir <- dirname(slides_rmd)
      out.fname <- paste0(
          sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(slides_rmd)), 
          "_share_image.png")
      out.fname <- file.path(out.dir, out.fname)
    }
    
    rmd_yaml <- rmarkdown::yaml_front_matter(slides_rmd)

    if (is.null(rmd_yaml[["output"]][[1]][["nature"]][["seal"]])) {
      # Use cfaXaringan 'default' title slide
      rmd_yaml[["output"]][[1]][["include"]] <- NULL
      rmd_yaml[["output"]][[1]][["nature"]] <-
      rmd_yaml[["output"]][[1]][["extras"]] <-
      rmd_yaml[["output"]][[1]][["include"]] <- 'null' # need to strip quotes...

      tmp_rmd <- tempfile(pattern = "scrshot", fileext = ".Rmd")
      cat(c("---", yaml::as.yaml(rmd_yaml), "---"), sep = "\n", file = tmp_rmd)
      rmd_txt <- readLines(tmp_rmd)
      writeLines(gsub("'null'", "null", rmd_txt), tmp_rmd)
    } else {
      # could try to find slide with "class: .title-slide"
      # for now figure it will be in the first 3 slides (+keep yaml)
      rmd_txt <- readLines(slides_rmd)
      tmp_rmd <- tempfile(pattern = "scrshot", fileext = ".Rmd")
      fence.index <- grep("---", rmd_txt)
      slide.subset <- min(length(fence.index), 5)
      writeLines(rmd_txt[seq_len(fence.index[slide.subset])], tmp_rmd)
    }

    webshot2::rmdshot(
      doc = tmp_rmd,
      file = out.fname,
      vwidth = width,
      vheight = height,
      rmd_args = list(
      output_options = list(
        nature = list(ratio = ratio),
        self_contained = TRUE))
    )

    out.fname
}
