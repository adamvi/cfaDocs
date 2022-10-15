#' @title createReportScripts
#' @description Create parent .Rmd and auxilary files for rendering
#'  CFA themed reports.
#' @param params List. A named list with parameter elements to be turned into
#'  YAML and RMD master/parent files in the creation of a CFA report.
#' @param content List. A named list of RMD child documents to be included in
#'  the report. Additional information such as file order and paths also included.
#' @param pagedown Logical. Should pagedown files (master/parent named by
#'  report/appendix title) be created. Default: TRUE
#' @param bookdown Logical. Should bookdown files (\sQuote{_bookdown_site.yml}
#'  and \sQuote{index.Rmd}) be created. Default: FALSE
#' @return Returns one or more .Rmd and/or .yml files that contain the required
#'  report meta-data and child file structure/order to produce an CFA themed
#'  report/bookdown site.
#' @details Create master/parent documents appropriate for use with the CFA
#'  RMarkdown template. Lists provided in the \code{params} and \code{content}
#'  arguments are typically constructed from custom and/or universal elements.
#'  Standard .Rmd files are also included in the package (such as a \code{R}
#'  session information script for use in an appendix or within a report).
#' @examples 
#' \dontrun{
#'   createReportScripts(
#'      params = report.params.list
#'      content = report.content.file.list
#'   )
#' }
#' @seealso 
#'  \code{\link[ymlthis]{yml}}, \code{\link[ymlthis]{yml_author}}, \code{\link[ymlthis]{yml_citations}}, \code{\link[ymlthis]{yml_params}}, \code{\link[ymlthis]{yml_bookdown_opts}}, \code{\link[ymlthis]{yml_output}}, \code{\link[ymlthis]{yml_pagedown_opts}}
#'  \code{\link[cfaDocs]{cfa_paged}}
#' @rdname createReportScripts
#' @export 
#' @importFrom ymlthis yml_empty yml_title yml_subtitle yml_author yml_date yml_toplevel yml_citations yml_params yml_bookdown_opts yml_output yml_pagedown_opts
createReportScripts =
    function(
        params,
        content,
        pagedown=TRUE,
        bookdown=FALSE
    ) {
    ###     Determine file order and paths before any YAML

    if (length(content$report$file.order) !=
        length(
            c(content$report.source$package,
              content$report.source$universal,
              content$report.source$custom,
              content$report.source$src.code
            )
        )
    ) {
        stop("Length of content$report$file.order and all content$report.source must be the same!")
    }
    if (length(
        intersect(
            c(content$report.source$package,
              content$report.source$universal
            ),
            c(content$report.source$custom,
              content$report.source$src.code
            )
        )
    ) != 0) {
        stop("There is overlap between content$report.source$universal/package and content$report.source$custom")
    }
    if (is.null(content$report$references)) {
        content$report$references <- FALSE
    }

    tmp.report.paths <-
        vector(
            mode = "character",
            length = length(content$report$file.order)
        )
    tmp.report.paths[content$report.source$package] <-
        params$params$package.rmd.path
    tmp.report.paths[content$report.source$universal] <-
        params$params$unvrsl.rmd.path
    tmp.report.paths[content$report.source$custom] <-
        params$params$custom.rmd.path
    tmp.report.paths[content$report.source$src.code] <-
        params$params$source.code.path
    report.files <-
        file.path(tmp.report.paths, content$report$file.order)

    if (length(content$appendices)) {
        appdx.tf <- TRUE
        appdx.files <- list()
        if (is.null(params$params$package.rmd.path)) {
            params$params$package.rmd.path <- cfa_shared_res("rmd")
        }
        for (apdx in seq(content$appendices)) {
            tmp.appdx.paths <-
                vector(
                    mode = "character",
                    length = length(content$appendices[[apdx]]$file.order)
                )
            tmp.appdx.paths[content$appendices[[apdx]]$appndx.source$package] <-
                params$params$package.rmd.path
            tmp.appdx.paths[content$appendices[[apdx]]$appndx.source$universal] <-
                params$params$unvrsl.rmd.path
            tmp.appdx.paths[content$appendices[[apdx]]$appndx.source$custom] <-
                params$params$custom.rmd.path
            tmp.appdx.paths[content$appendices[[apdx]]$appndx.source$src.code] <-
                params$params$source.code.path
            appdx.files[[apdx]] <-
                file.path(tmp.appdx.paths, content$appendices[[apdx]]$file.order)
        }
    } else {
        appdx.tf <- FALSE
    }

    ###     Output information
    if (is.null(params$output$bookdown$directory)) {
        bd.output.dir <- "site"
    } else {
        bd.output.dir <- params$output$bookdown$directory
    }

    if (is.null(params$output$pagedown$directory)) {
        pd.output.dir <- "report"
    } else {
        pd.output.dir <- params$output$pagedown$directory
    }
    ##      Determine pagedown file name (can be supplied or automatic based on title)
    if (is.null(params$output$pagedown$file)) {
        pd.output.file <- gsub(" ", "_", params$top.level$title)
    } else {
        pd.output.file <- params$output$pagedown$file
    }

    ###     Top-level YAML used in both bookdown and pagedown
    top.yml <-
        ymlthis::yml_empty() |>
          ymlthis::yml_title(addQuote(params$top.level$title)) |>
            ymlthis::yml_subtitle(addQuote(params$top.level$subtitle)) |>
              ymlthis::yml_author(
                    name=addQuote(params$top.level$author.names),
                    affiliation=addQuote(params$top.level$author.affil)
              ) |>
                ymlthis::yml_date(
                    ifelse(
                        test = is.null(params$top.level$date),
                        yes = format(Sys.time(), format = "%B %Y"),
                        no = addQuote(params$top.level$date)
                    )
                )

    #####
    ###     Bookdown files
    #####

    ###     index.Rmd
    if (bookdown) {
        index.rmd.yml <- top.yml |>
            ymlthis::yml_toplevel(favicon = cfa_shared_res("img", "cfa-long-logo.svg"))
        if (params$top.level$executive.summary) {
            index.rmd.yml <- ymlthis::yml_toplevel(.yml = index.rmd.yml, `abstract-title`= "Executive Summary")
        }

        bd.downloads <- paste0("[\"downloads/", paste0(pd.output.file, ".pdf"), "\", \"Report\"]")
        if (appdx.tf) {
            for (apdx in seq(content$appendices)) {
                tmp.apdx.label <- names(content$appendices[apdx])
                bd.downloads <- c(bd.downloads, paste0("[\"downloads/", paste0(gsub(" ", "_", content$appendices[[apdx]]$title), "_APPENDIX_",    tmp.apdx.label, ".pdf"), "\", \"Appendix ",    tmp.apdx.label, "\"]"))
            }
        } # paste0("downloads/", paste0pd.output.file, ".html"))) # Add link to HTML formats?
        if (length(bd.downloads) == 1L) bd.downloads <- paste0("[", bd.downloads, "]")
        ##    Scoping issue when trying to send in objects like 'params$top.level$title' with ymlthis::yml_output(bookdown::gitbook(...))

        index.rmd.yml <- index.rmd.yml |>
            ymlthis::yml_toplevel(
                list(output = list(
                    `bookdown::gitbook` = list(
                        fig_caption = FALSE,
                        css = "assets/css/literasee_bookdown_style.css",
                        config = list(
                            toc = list(
                                before = paste0("<li><a href=\"./\"><strong>", params$top.level$title, "</strong></a></li>"),
                                after =  paste0("<li><a href=\"", params$client.info$github.repo, "\" target=\"blank\">Analysis Code on Github</a></li>")),
                            download = bd.downloads),
                        split_bib = FALSE)
                    )
                )
            ) |>
                ymlthis::yml_citations(
                    bibliography = params$top.level$bibliography,
                    link_citations = TRUE
                )

        ##        clean up (nested) params
        tmp.params <- params$params
        for (prm in seq(tmp.params)) {
            if (is.character(tmp.params[[prm]])) tmp.params[[prm]] <- addQuote(tmp.params[[prm]])
            if (is.list(tmp.params[[prm]])) {
                tmp.params[[prm]] <- gsub("'c[(]", "c(", gsub("[)]'", ")", paste0("!r list(", paste(names(tmp.params[[prm]]), "=", addQuote(tmp.params[[prm]]), collapse = ", "), ")XXX")))
            }
        }
        # index.rmd.yml <- do.call(ymlthis::yml_params, c(list(.yml = index.rmd.yml), sapply(params$params, function(f) ifelse(is.character(f), addQuote(f), f)), render.format = "'bookdown'"))
        index.rmd.yml <-
            do.call(
                ymlthis::yml_params,
                c(list(.yml = index.rmd.yml), tmp.params, render.format = "'bookdown'")
            )

        writeYAML(yaml = index.rmd.yml, filename = "index.Rmd")
        addCurrentDraft(config = params, filename = "index.Rmd")
        # Make params and setup child RMD files to be included in the content/rmd.files argument/object
        # addChildChunk(rmd_file = file.path(params$params$unvrsl.rmd.path[[1]], "report_setup.Rmd"),
        #               comments = "Set up report params, packages, cache, etc.",
        #               filename = "index.Rmd")
        cat("\n# {.unlisted .unnumbered}\n", file = "index.Rmd", append = TRUE)

        ###     Create _bookdown.yml for website/book
        if (!is.null(content$bookdown$file.order)) {
            bd.files <- report.files[content$bookdown$file.order]
        } else {
            bd.files <- report.files
        }

        bd.files <- c(
            ifelse(
                test = file.exists("index.Rmd"),
                yes = "index.Rmd",
                no = ""
            ),
            bd.files,
            ifelse(
                test = content$report$references,
                yes = cfa_paged_res("cfa", "rmd", "references.Rmd"),
                no = ""
            )
        )
        bd.files <- bd.files[bd.files != ""]
        if (appdx.tf) { # ifelse doesn't work with character vectors length > 1
            if (length(content$appendices) > 1) {
                apdx.placeholder <- "appendices.Rmd"
            } else apdx.placeholder <- "appendix.Rmd"
            bd.files <-
                c(bd.files,
                  cfa_paged_res("cfa", "rmd", apdx.placeholder),
                  setdiff(unlist(appdx.files), report.files)
                ) # setdiff to weed out params.Rmd, setup.Rmd, etc. (possibly needed for pagedown)
        }

        extra.bd.files <-
            grep("references.Rmd|appendix.Rmd|appendices.Rmd",
                 list.files(content$bookdown$rmd.path),
                 invert=TRUE, value = TRUE
            )
        if (length(extra.bd.files)) {
            bd.files <- c(bd.files, file.path(content$bookdown$rmd.path, extra.bd.files))
        }

        ymlthis::yml_empty() |>
            ymlthis::yml_bookdown_opts(
                output_dir = bd.output.dir,
                delete_merged_file = TRUE,
                rmd_files = addQuote(bd.files) # paste0("[\n", paste0(addQuote(bd.files), collapse = ",\n"), "\n]")
            ) |>
                writeYAML(filename = "_bookdown.yml", fences=FALSE, remove_scalar_style=TRUE)
    }


    #####
    ###     Pagedown files
    #####

    if (pagedown) {
        pgdwn.rmd.yml <- top.yml |>
            ymlthis::yml_toplevel(
                knit= "pagedown::chrome_print",
                client_city = params$client.info$city.name,
                client_state = params$client.info$state.abv,
                client_organization = addQuote(params$client.info$organization)
            )

        if (!is.null(params$client.info$org.head)) {
            pgdwn.rmd.yml <-
                ymlthis::yml_toplevel(
                    .yml = pgdwn.rmd.yml,
                    client_name = addQuote(params$client.info$org.head)
                )
        }
        if (!is.null(params$top.level$project.team)) {
            pgdwn.rmd.yml <-
                ymlthis::yml_toplevel(
                    .yml = pgdwn.rmd.yml,
                    project_team = addQuote(params$top.level$project.team)
                )
        }
        if (!is.null(params$client.info$github.repo)) {
            pgdwn.rmd.yml <-
                ymlthis::yml_toplevel(
                    .yml = pgdwn.rmd.yml,
                    project_code = addQuote(paste0("[Github](", params$client.info$github.repo, ")"))
                )
        }
        if (!is.null(params$top.level$project.email)) {
            pgdwn.rmd.yml <-
                ymlthis::yml_toplevel(
                    .yml = pgdwn.rmd.yml,
                    project_email = params$top.level$project.email
                )
        }
        if (!is.null(params$client.info$acknowledgements)) {
            pgdwn.rmd.yml <-
                ymlthis::yml_toplevel(
                    .yml = pgdwn.rmd.yml,
                    acknowledgements = addQuote(params$client.info$acknowledgements)
                )
        }
        pgdwn.rmd.yml <- pgdwn.rmd.yml |>
            ymlthis::yml_output(
                cfa_paged(
                    fig_caption = TRUE,
                    template = "default"
                )
            ) |>
                ymlthis::yml_toplevel(
                    lof = TRUE,
                    `lof-title` = addQuote("Tables and Figures")
                ) |>
                    ymlthis::yml_pagedown_opts(
                        toc = TRUE,
                        toc_title = addQuote("Report Contents"),
                        paged_footnotes = TRUE
                    )
        if (content$report$references) {
            pgdwn.rmd.yml <- pgdwn.rmd.yml |>
                 ymlthis::yml_citations(
                     bibliography = params$top.level$bibliography,
                     link_citations = TRUE)
        }

        ##        clean up (nested) params
        if (!exists("tmp.params")) {
            tmp.params <- params$params
            for (prm in seq(tmp.params)) {
                if (is.character(tmp.params[[prm]])) tmp.params[[prm]] <- addQuote(tmp.params[[prm]])
                if (is.list(tmp.params[[prm]])) {
                    tmp.params[[prm]] <- gsub("'c[(]", "c(", gsub("[)]'", ")", paste0("!r list(", paste(names(tmp.params[[prm]]), "=", addQuote(tmp.params[[prm]]), collapse = ", "), ")XXX")))
                }
            }
        }
        pgdwn.rmd.yml <-
            do.call(
                ymlthis::yml_params,
                c(list(.yml = pgdwn.rmd.yml), tmp.params, render.format = "pagedown")
            )
        abstract.child <-
            eval(parse(text = 
                paste0(
                    "ymlthis::code_chunk(chunk_args = list(child = '",
                    cfa_paged_res("cfa", "rmd", "abstract.Rmd"),
                    "'))"
                )
            ))
        pgdwn.rmd.yml <-
            ymlthis::yml_toplevel(
                .yml = pgdwn.rmd.yml,
                abstract = abstract.child
            )

        pd.filename <- file.path(pd.output.dir, paste0(pd.output.file, ".Rmd"))
        if (!dir.exists(pd.output.dir)) dir.create(pd.output.dir)
        writeYAML(yaml = pgdwn.rmd.yml, filename = pd.filename)
        addCurrentDraft(config=params, filename = pd.filename)

        ###     Add in content child chunks
        if (!is.null(content$pagedown$file.order)) {
            pgd.files <- report.files[content$pagedown$file.order]
        } else {
            pgd.files <- report.files
        }
        for (chld in pgd.files) {
            ##    Move child files back an additional level ("..") since rendered from the "./report/" directory
            ##    Not done if a cfaDocs package (shared) resource - provide users' absolute R library path
            tmp.leader <-
                ifelse(
                    agrepl(cfa_shared_res("rmd"), chld),
                    "",
                    ".."
                )
            if (toupper(fileExtension(basename(chld))) == ".RMD") {
                addChildChunk(
                    rmd_file = file.path(tmp.leader, chld),
                    label = getChunkLabel(basename(chld)),
                    filename = pd.filename
                )
            } else {
                addKnitSpinChunk(
                    r_src_file = file.path(tmp.leader, chld),
                    label = getChunkLabel(basename(chld)),
                    filename = pd.filename
                )
            }
        }

        if (content$report$references) {
            addReferences(pd.filename)
        }

        ###     Pagedown appendices
        if (appdx.tf) {
            for (apdx in seq(content$appendices)) {
                tmp.apdx.label <- names(content$appendices[apdx])
                if (!is.null(content$appendices[[apdx]]$table.of.contents)) {
                    if (is.logical(content$appendices[[apdx]]$table.of.contents)) {
                        apdx.toc <- content$appendices[[apdx]]$table.of.contents
                    } else {
                        apdx.toc <- TRUE
                    }
                } else {
                    apdx.toc <- FALSE
                }
                if (!is.null(content$appendices[[apdx]]$list.of.figures)) {
                    if (is.logical(content$appendices[[apdx]]$list.of.figures)) {
                        apdx.lof <- content$appendices[[apdx]]$list.of.figures
                    } else {
                        apdx.lof <- TRUE
                    }
                } else {
                    apdx.lof <- FALSE
                }
                if (!is.null(content$appendices[[apdx]]$references)) {
                    if (is.logical(content$appendices[[apdx]]$references)) {
                        apdx.refs <- content$appendices[[apdx]]$references
                    } else {
                        apdx.refs <- TRUE
                    }
                } else {
                    apdx.refs <- FALSE
                }
                tmp.appdx.yml <-
                    ymlthis::yml_empty() |>
                        ymlthis::yml_title(
                            addQuote(content$appendices[[apdx]]$title)
                        ) |>
                        ymlthis::yml_subtitle(
                            addQuote(params$top.level$title)
                        ) |>
                        ymlthis::yml_toplevel(
                            `appendix-prefix` = addQuote(tmp.apdx.label)
                        ) |>
                        ymlthis::yml_output(
                            cfa_paged(
                                fig_caption = TRUE,
                                template = "appendix"
                            )
                        ) |>
                        ymlthis::yml_pagedown_opts(paged_footnotes = TRUE)
                if (apdx.toc) {
                    tmp.appdx.yml <- tmp.appdx.yml |>
                        ymlthis::yml_pagedown_opts(
                            toc = TRUE,
                            toc_title = addQuote("Appendix Contents")
                        )
                } else {
                    tmp.appdx.yml <- tmp.appdx.yml |>
                        ymlthis::yml_pagedown_opts(toc = FALSE)
                }
                if (apdx.lof) {
                    tmp.appdx.yml <- tmp.appdx.yml |>
                        ymlthis::yml_toplevel(
                            lof = TRUE,
                            `lof-title` = addQuote("Tables and Figures")
                        )
                }
                if (apdx.refs) {
                    tmp.appdx.yml <- tmp.appdx.yml |>
                         ymlthis::yml_citations(
                             bibliography = params$top.level$bibliography,
                             link_citations = TRUE)
                }

                ##        clean up (nested) params
                if (!exists("tmp.params")) {
                    tmp.params <- params$params
                    for (prm in seq(tmp.params)) {
                        if (is.character(tmp.params[[prm]])) {
                            tmp.params[[prm]] <- addQuote(tmp.params[[prm]])
                        }
                        if (is.list(tmp.params[[prm]])) {
                            tmp.params[[prm]] <-
                                gsub("'c[(]", "c(",
                                    gsub("[)]'", ")",
                                        paste0("!r list(",
                                            paste(names(tmp.params[[prm]]), "=", addQuote(tmp.params[[prm]]), collapse = ", "),
                                            ")XXX"
                                        )
                                    )
                                )
                        }
                    }
                }

                tmp.appdx.yml <-
                    do.call(
                        ymlthis::yml_params,
                        c(list(.yml = tmp.appdx.yml), tmp.params, render.format = "pagedown")
                    )
                if (!is.na(tmp.apdx.label)) {
                    tmp.apdx.fname <-
                        file.path(
                            pd.output.dir,
                            paste0(
                                gsub(" ", "_", content$appendices[[apdx]]$title),
                                "_APPENDIX_",
                                tmp.apdx.label,
                                ".Rmd"
                            )
                        )
                } else {
                    tmp.apdx.fname <-
                        file.path(
                            pd.output.dir,
                            paste0(
                                gsub(" ", "_", content$appendices[[apdx]]$title),
                                "_APPENDIX.Rmd"
                            )
                        )
                }

                writeYAML(yaml = tmp.appdx.yml, filename = tmp.apdx.fname)
                addCurrentDraft(config=params, filename = tmp.apdx.fname)

                tmp.code_chunk <-
                    paste0(
                        "require(ymlthis)\n\tymlthis::yml_empty() |>\n\t\tymlthis::yml_bookdown_opts(\n\t\t\tlanguage=list(label=list(fig=\"'Figure ",
                        tmp.apdx.label,
                        "'\", tab=\"'Table ",
                        tmp.apdx.label,
                        "'\"))\n\t\t) |>\n\t\t\tcfaDocs:::writeYAML(filename = '_bookdown.yml', fences = FALSE)"
                    )
                addCodeChunk(
                    chunk_args = "include = FALSE",
                    code_chunk = tmp.code_chunk,
                    comments = "create _bookdown.yml file for labeling `Figures` and `Tables` with appendix prefix.",
                    filename = tmp.apdx.fname
                )

                ###     Add in content child chunks
                for (apdxchld in appdx.files[[apdx]]) {
                    tmp.leader <-
                        ifelse(
                            agrepl(cfa_shared_res("rmd"), apdxchld),
                            "",
                            ".."
                        )
                    if (toupper(fileExtension(basename(apdxchld))) == ".RMD") {
                        addChildChunk(
                            rmd_file = file.path(tmp.leader, apdxchld),
                            label = getChunkLabel(basename(apdxchld)),
                            filename = tmp.apdx.fname
                        )
                    } else {
                        addKnitSpinChunk(
                            r_src_file = file.path(tmp.leader, apdxchld),
                            label = getChunkLabel(basename(apdxchld)),
                            filename = tmp.apdx.fname
                        )
                    }
                }

                tmp.code_chunk <-
                    "options(table_counter_str = NULL)\n\toptions(table_num_str = NULL)\n\toptions(fig_caption_no_sprintf = NULL)\n\toptions(fig_num_str = NULL)"
                addCodeChunk(
                    chunk_args = "cache = FALSE, results = 'asis', echo = FALSE",
                    code_chunk = tmp.code_chunk,
                    comments = "End appendix format: re-start counters and change back to numeric for subsequent re-rendering",
                    filename = tmp.apdx.fname
                )
                if (apdx.refs) {
                    addReferences(tmp.apdx.fname)
                }
            }
        }
    }
}
