
r_session_info <- devtools::session_info()
session_platform <- data.frame(unlist(r_session_info$platform))
names(session_platform) <- "Value"
session_platform$Setting <- rownames(session_platform)
row.names(session_platform) <- NULL

trim_source <- function(txt){
    split.at <- strsplit(txt, "@")[[1]]
    git.commit <- substr(split.at[2], 1, 7)
    if (!is.na(git.commit)) {
    paste0("    ", split.at[1], "@", git.commit, ")")
    } else txt
}

session_packages <- data.frame(r_session_info$packages, row.names = NULL)

nmspace_pkgs <- subset(session_packages, attached == FALSE, 
                        select = c(package, loadedversion))
nmspace_pkgs$alph <- toupper(unlist(lapply(nmspace_pkgs$package, substr, 1, 1)))
namespace_pkgs <- NULL
for (n in unique(nmspace_pkgs$alph)) {
    chnk <- subset(nmspace_pkgs, alph == n)
    chnk <- paste0("**", chnk$package, "**", " *(", chnk$loadedversion, ")*")
    chnk <- paste(chnk, collapse = ", ")
    namespace_pkgs <- c(namespace_pkgs, chnk)
}

attached_pkgs <- subset(session_packages, attached == TRUE, 
                        select = c(package, loadedversion, source, date))
attached_pkgs$source <- unlist(lapply(attached_pkgs$source, trim_source))
attached_pkgs$date <- format(as.Date(attached_pkgs$date), "%m/%d/%Y")
names(attached_pkgs) <- c("Package", "Version", "Source", "Date Installed")
row.names(attached_pkgs) <- NULL