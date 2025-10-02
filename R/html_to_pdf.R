
html_to_pdf <-
  function(
    urls, delay = 1, ...
  ) {
    CS <- chromote::ChromoteSession$new()

    for (u in urls) {
      CS$go_to(u, delay = delay)
      p <-
        CS$Page$printToPDF(
          printBackground = TRUE,
          preferCSSPageSize = TRUE,
          ..., wait_ = TRUE
        )$data

      filename <- basename(u) |>
        gsub(".html", ".pdf", x = _) |>
        gsub("/$", "", x = _) |>
        fs::path_sanitize(replacement = "_")

      outfile <- file(filename, "wb") # wb = write binary
      base64enc::base64decode(p, output = outfile)
      close(outfile)
    }

    invisible(CS$close())
  }