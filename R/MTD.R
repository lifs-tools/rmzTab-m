## Code related to import/export of the MTD element

#' @description
#'
#' Helper function to create the CV entrie(s) for an mzTab-M file.
#'
#' @note all paramters have to have the same length.
#' 
#' @param label `character` with the label of the CV(s)
#'
#' @param full_name `character` with the name of the CV(s)
#'
#' @param version `character` with the version of the CV(s)
#'
#' @param uri `character`
#'
#' @return two column `character` `matrix`.
#'
#' @author Philippine Louail, Johannes Rainer
#'
#' @noRd
.cv <- function(label = character(), full_name = character(),
                version = character(), uri = character()) {
    if (!length(label)) return(matrix(NA_character_, ncol = 2, nrow = 0))
    if (length(unique(c(length(label), length(full_name),
                        length(version), length(uri)))) > 1)
        stop("CV: different number of elements provided.", call. = FALSE)
    l <- seq_along(label)
    res <- cbind(c(paste0("cv[", l, "]-label"),
                   paste0("cv[", l, "]-full_name"),
                   paste0("cv[", l, "]-version"),
                   paste0("cv[", l, "]-uri")),
                 c(label, full_name, version, uri),
                 order = .prefix_zero(rep(l, 4))
                 )
    res[order(res[, "order"]), 1:2]
}

#' @title Create a skeleton MTD section
#'
#' @description
#'
#' Create a `matrix` with the basic mzTab-M MTD section based on the provided
#' data. The returned result contains only minimal information. It should be
#' expanded, corrected and completed with additional fields and information.
#'
#' @return two-column `character` `matrix`.
#'
#' @author Philippine Louail, Johannes Rainer
#' 
#' @noRd
.mtd_skeleton <- function(id = character(),
                          software = character(),
                          quantification_method = "[MS, MS:1001834, LC-MS label-free quantitation analysis, ]",
                          cv_label = c("MS", "PRIDE")
                          cv_full_name = c("PSI-MS controlled vocabulary", "PRIDE PRoteomics IDEntifications (PRIDE) database controlled vocabulary")
                          cv_version = c("4.1.138", "16:10:2023 11:38")
                          cv_uri = c("https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo", "https://www.ebi.ac.uk/ols/ontologies/pride")
                          database = "[,, \"no database\", null ]",
                          database_prefix = "null",
                          database_version = "Unknown",
                          database_uri = "null") {
    if (!length(id)) stop("Parameter 'id' is required")
    ## call .cv...
    ## WORK IN PROGRESS
}
