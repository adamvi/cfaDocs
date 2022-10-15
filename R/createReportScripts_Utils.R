#' @importFrom ymlthis last_yml
#' @importFrom yaml as.yaml

writeYAML =
  function(
    yaml = ymlthis::last_yml(),
    filename,
    fences = TRUE,
    remove_scalar_style = FALSE,
    append_yaml = FALSE
  ) {
    clean.yaml <- gsub("'''", "\"", yaml::as.yaml(yaml, line.sep = "\n", column.major = FALSE))
    clean.yaml <- gsub("''", "\"", clean.yaml)
    clean.yaml <- gsub("'[[]", "[", clean.yaml)
    clean.yaml <- gsub("[]]'", "]", clean.yaml)
    clean.yaml <- gsub("\n\n", "\n", clean.yaml)
    clean.yaml <- gsub("'TRUE'", "yes", clean.yaml)
    clean.yaml <- gsub("'FALSE'", "no", clean.yaml)
    clean.yaml <- gsub("\"NULL\"", "c()", clean.yaml)
    clean.yaml <- gsub("'[!]r", "!r", clean.yaml)
    clean.yaml <- gsub(")XXX'", ")", clean.yaml)
    if (remove_scalar_style) {
      clean.yaml <- gsub("[|]-\n", "", clean.yaml)
    }
    if (fences) {
      cat(paste0("---\n", clean.yaml, "---\n"), file = filename, append = append_yaml)
    } else {
      cat(clean.yaml, file = filename, append = append_yaml)
    }
}


addQuote = function(text) {
  paste0("'", text, "'")
}


addCurrentDraft = function(config, filename) {
  auth.list <- config$top.level$author.names
  cat(paste0("
<!--
  This document was written by ", paste(paste(head(auth.list, -1), collapse = ", "), tail(auth.list, 1), sep = " and "), " for the ", config$client.info$organization, "\n\t",
  "Current Draft:  ", format(Sys.time(), format = "%B %d, %Y"), "\n",
"-->\n\n"), file = filename, append = TRUE)
}


addChildChunk =
  function(
    rmd_file,
    comments = NULL,
    label = NULL,
    filename
  ) {
  if (!is.null(comments)) {
    for (cmnt in comments) {
      cat(
        paste0("<!--  ", cmnt, "-->\n"),
        file = filename, append = TRUE
      )
    }
  }
  cat(paste0("
```{r ", label, ", child = '", rmd_file, "'}\n```\n"),
    file = filename, append = TRUE
  )
}


addKnitSpinChunk =
  function(
    r_src_file,
    comments = NULL,
    label = NULL,
    filename
  ) {
  if (!is.null(comments)) {
    for (cmnt in comments) {
      cat(paste0("<!--  ", cmnt, "-->\n"),
          file = filename, append = TRUE
      )
    }
  }
  cat(
    paste0("
```{r ", label, ", echo = FALSE}\n",
"  knitr::spin_child('", r_src_file, "')\n",
"```\n"
    ),
    file = filename, append = TRUE
  )
}


addCodeChunk =
  function(
    chunk_args,
    code_chunk,
    comments = NULL,
    label = NULL,
    filename
  ) {
  if (!is.null(comments)) {
    for (cmnt in comments) {
      cat(paste0("\n<!--  ", cmnt, "-->\n"), file = filename, append = TRUE)
    }
  }
  cat(paste0("
```{r, ", label, chunk_args, "}\n\t", code_chunk, "\n```\n"), file = filename, append = TRUE)
}


addReferences = function(filename) {
  cat("
<!--  References  -->

# References {-}
::: {#refs}
:::\n", file = filename, append = TRUE)
}


getChunkLabel =
  function(x) {
    if (is.null(x)) return(NULL)
    if (is.na(x)) return(NA)
    if (identical(x, "-")) return("-")
    x <- gsub(" ", "-", x)
    x <- gsub("_", "-", x)
    x <- gsub("[.]", "-", x)
    x <- gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
    x <- gsub("[(]", "( ", x)
    x <- gsub("[)]", " )", x)

    if (identical(x, "")) {
      return("")
    } else {
      s <- unlist(lapply(strsplit(x, split = "-"), tolower))
      paste(s, collapse = "-")
    }
} ### END getChunkLabel
