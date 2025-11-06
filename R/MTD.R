## Code related to import/export of the MTD element

#' @title Prepare and format information for the mzTab-M metadata section
#'
#' @description
#'
#' This function assists in creating and formatting information for the
#' mzTab-M metadata section (MTD). It combines and formats the provided input
#' values for a specific
#'
#' See [mzTab-M documentation](https://github.com/HUPO-PSI/mzTab-M/blob/main/specification_documents/mzTab_format_specification_2_1-M.adoc#62-metadata-section)
#' for more information, examples and expected format.
#' 
#' @param ... `character` vector(s), each having the same length, with
#'     the values for the field(s). Parameter `field_prefix` defines the name
#'     of the field (e.g., `"cv"`). If names are provided (e.g.
#'     `label = "my label"`), these are appended to the `field_prefix` (e.g.,
#'     for `field_prefix = "cv"`, the returned field's name is combined to
#'     `"cv[1]-label"`).
#'
#' @param field_prefix `character(1)` defining the prefix to be used
#'     (e.g., `prefix = "cv"`). This is the prefix/first part of the field's
#'     name.
#'
#' @return two column `character` `matrix` with the formatted elements.
#'
#' @author Johannes Rainer, Philippine Louail
#'
#' @examples
#'
#' ## Define the CV element with 3 CV terms:
#' mtd_fields(
#'     label = c("a", "b", "c"),
#'     full_name = c("A", "B", "C"),
#'     version = c(1, 2, 3),
#'     uri = c("u1", "u2", "u3"),
#'     field_prefix = "cv")
#'
#' ## Define a single software:
#' mtd_fields("[MS, MS:1002879, Progenesis QI, 3.0]", field_prefix = "software")
#'
#' ## Define two softwares:
#' mtd_fields(c("[MS, MS:1002879, Progenesis QI, 3.0]", "[a, b, c, d]"),
#'     field_prefix = "software")
#'
#' ## Define a software with the optional setting
#' mtd_fields(c("[MS, MS:1002879, Progenesis QI, 3.0]", "[a, b, c, d]"),
#'     `setting[1]` = c("my cool settings", "none"),
#'     `setting[2]` = c("other setting", "none"),
#'     field_prefix = "software")
#'
#' ## Define database fields
#' mtd_fields(
#'    c("[MITIAM, MRI:00100079, HMDB, ]", "[,, de novo, ]"),
#'    prefix = c("hmdb", "dn"),
#'    version = c("3.6", "Unknown"),
#'    uri = c("http://www.hmdb.ca", "null"),
#'    field_prefix = "database"
#' )
#'
#' @export
mtd_fields <- function(..., field_prefix = "") {
    dots <- list(...)
    ls <- lengths(dots)
    if (length(unique(ls)) > 1)
        stop(field_prefix, ": number of provided elements must match",
             call. = FALSE)
    n <- names(dots)
    if (is.null(n)) n <- ""
    n[n != ""] <- paste0("-", n[n != ""])
    l <- seq_len(ls[1L])
    res <- cbind(
        paste0(field_prefix, "[", rep(l, length(n)), "]",
               rep(n, each = ls[1L])),
        unlist(dots, use.names = FALSE),
        .prefix_zero(rep(l, length(n)))
    )
    res[order(res[, 3L]), 1:2]
}

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
    mtd_fields(label = label, full_name = full_name, version = version,
               uri = uri, field_prefix = "cv")
}

.database <- function(database = character(), prefix = character(),
                      version = character(), uri = character()) {
    if (!length(database)) return(matrix(NA_character_, ncol = 2, nrow = 0))
    mtd_fields(database, prefix = prefix, version = version, uri = uri,
               field_prefix = "database")
}

#' @title Create a skeleton MTD section
#'
#' @description
#'
#' Create a `matrix` with the basic mzTab-M *MTD* section based on the provided
#' data. The returned result contains only minimal information. It should be
#' expanded, corrected and completed with additional fields and information
#' (i.e., the *skeleton* returned by this function should be completed with
#' *flesh*).
#'
#' For details and expected input for the various parameter it is **strongly
#' suggested** to consult the [mzTab-M](https://github.com/HUPO-PSI/mzTab-M/blob/main/specification_documents/mzTab_format_specification_2_1-M.adoc#62-metadata-section) documentation.
#'
#' @param id `character(1)` (**mandatory**) with the ID of the data set.
#'
#' @param software `character` (**mandatory**) with the software(s) used.
#'     Can be of length > 1 if multiple softwares were used.
#'
#' @param quantification_method `character(1)` defining the quantification
#'     method used in the experiment.
#'
#' @param cv_label `character` describing the labels of the controlled
#'     vocabularies/ontologies used in the mzTab file as a short-hand, e.g.
#'     `cv_label = "MS"` for PSI-MS.
#'
#' @param cv_full_name `character` with the full names of the controlled
#'     vocabularies/ontologies used in the mzTab file.
#'
#' @param cv_version `character` with the version of the used
#'     vocabularies/ontologies.
#'
#' @param cv_uri `character` with the URIs of the vocabularies/ontologies.
#'
#' @param database `character` defining the database used for annotation. If no
#'     annotation/identification was performed then `"[,, no database, null]"`
#'     should be used.
#'
#' @param database_prefix `character` defining the prefix used in the
#'     *identifier* column of data tables. For *no database*, `"null"` must
#'     be used.
#'
#' @param database_version `character` with the database version used.
#'
#' @param database_uri `character` with the URI to the database(s). For
#'     *no database* `"null"` must be used.
#'
#' @param small_molecule_quantification_unit `character(1)` defines the type
#'     of units are reported in the small molecule summary quantification/
#'     abundance fields.
#'
#' @param small_molecule_feature_quantification_unit `character(1)` defines
#'     what type of units are reported in the small molecule feature
#'     quantification / abundance fields.
#'
#' @param small_molecule_identification_reliability `character(1)` defines the
#'     system used for giving reliability / confidence codes to small molecule
#'     identifications MUST be specified if not using the default codes.
#' 
#' @param mztab_version `character(1)` defining the mzTab-M version of the file.
#' 
#' @return two-column `character` `matrix` that should be expanded with
#'     additional fields (such as *title*, *description* etc) and
#'     information (with the help from the `mtd_fields()` function).
#'
#' @author Philippine Louail, Johannes Rainer
#'
#' @export
#' 
#' @examples
#'
#' ## Define a minimal mzTab-M metadata information
#' mtd <- mtd_skeleton(id = "001", software = "[MS, MS:1001582, xmcs, 4.0.0]")
#'
#' ## Column 1 has the field names
#' mtd[, 1]
#'
#' ## Column 2 the respective values
#' mtd[, 2]
#'
#' ## Add additional fields as defined in the mzTab-M definition
#' mtd <- rbind(
#'     mtd,
#'     c("title", "My simple xcms preprocessed data"),
#'     c("description", "A simple example xcms preprocessing."))
#'
#' tail(mtd)
#'
#' ## Add instrument information
#' instr <- mtd_fields(
#'     name = "[MS, MS:1000449, LTQ Orbitrap,]",
#'     source = "[MS, MS:1000073, ESI,]",
#'     `analyzer[1]` = "[MS, MS:1000291, linear ion trap,]",
#'     detector = "[MS, MS:1000253, electron multiplier,]",
#'     field_prefix = "instrument"
#' )
#' instr
#'
#' ## Add this information to the metadata
#' mtd <- rbind(mtd, instr)
#'
#' ## Define sample processing fields using the mtd_fields function
#' sp <- mtd_fields(
#'     c("[MSIO, MSIO:0000146, centrifugation,]",
#'       "[MSIO, MSIO:0000141, metabolite extraction,]",
#'       "[MSIO, MSIO:0000141, silylation,]"),
#'     field_prefix = "sample_processing")
#' sp
#'
#' ## Add this information to the metadata
#' mtd <- rbind(mtd, sp)
#' 
#' ## Since a new ontology was used for the sample processing, we need also to
#' ## add that to the metadata. We manually define the fields to add using
#' ## `"cv[3]"` because there are already 2 CVs defined in the MTD skeleton.
#' cv2 <- rbind(
#'     c("cv[3]-label", "MSIO"),
#'     c("cv[3]-full_name", "Metabolomics Standards Initiative Ontology"),
#'     c("cv[3]-version", "1.0.1"),
#'     c("cv[3]-uri", "http://purl.obolibrary.org/obo/msio.owl")
#' )
#'
#' ## Add this information to the metadata
#' mtd <- rbind(mtd, cv2)
#'
#' ## Finally sort the metadata fields according to the expected order
#' mtd <- mtd_sort(mtd)
#' mtd
mtd_skeleton <- function(id = character(),
                         software = character(),
                         quantification_method = "[MS, MS:1001834, LC-MS label-free quantitation analysis, ]",
                         cv_label = c("MS", "PRIDE"),
                         cv_full_name = c("PSI-MS controlled vocabulary", "PRIDE PRoteomics IDEntifications (PRIDE) database controlled vocabulary"),
                         cv_version = c("4.1.138", "16:10:2023 11:38"),
                         cv_uri = c("https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo", "https://www.ebi.ac.uk/ols/ontologies/pride"),
                         database = c("[,, \"no database\", null ]"),
                         database_prefix = c("null"),
                         database_version = c("Unknown"),
                         database_uri = c("null"),
                         small_molecule_quantification_unit = "[PRIDE, PRIDE:0000330, Arbitrary quantification unit, ]",
                         small_molecule_feature_quantification_unit = "[PRIDE, PRIDE:0000330, Arbitrary quantification unit, ]",
                         small_molecule_identification_reliability = "[MS, MS:1002896, compound identification confidence level, ]",
                         mztab_version = "2.0.0-M") {
    if (!length(id)) stop("Parameter 'id' is required", call. = FALSE)
    if (!length(software)) stop("Parameter 'software' is required", call.=FALSE)
    sk <- rbind(
        c("mzTab-version", mztab_version),
        c("mzTab-ID", "id"),
        mtd_fields(software, field_prefix = "software"),
        c("quantification_method", quantification_method),
        .cv(cv_label, cv_full_name, cv_version, cv_uri),
        .database(database, database_prefix, database_version, database_uri),
        c("small_molecule-quantification_unit",
          small_molecule_quantification_unit),
        c("small_molecule_feature-quantification_unit",
          small_molecule_feature_quantification_unit),
        c("small_molecule-identification_reliability",
          small_molecule_identification_reliability)
    )
    ## Add sample and run information...
    ## Order them.
    mtd_sort(sk)
}

#' Defines the order of the elements in MTD (pattern provided). This should
#' be used in a function that orders the MTD part of a mzTab-M file.
#'
#' @noRd
.MTD_FIELD_ORDER <- c(
    "mzTab-version",
    "mzTab-ID",
    "title",
    "description",
    "sample_processing",
    "instrument",
    "software",
    "publication",
    "contact",
    "uri",
    "external_study",
    "quantification",
    "sample",
    "ms_run",
    "assay",
    "study_variable",
    "custom",
    "cv",
    "database",
    "derivatization",
    "small_molecule-quantification",
    "small_molecule_feature",
    "small_molecule-identification",
    "id_confidence",
    "colunit-small_molecule",
    "colunit-small_molecule_feature",
    "colunit-small_molecule_evidence"
)

#' @title Sort rows in a MTD matrix to match the expected order
#'
#' @description
#'
#' Helper function to sort a mzTab-M *MTD* `matrix`, such as generated by
#' [mtd_skeleton()], into the correct order of the metadata fields.
#'
#' @param x two-column matrix with the first column containing the metadata
#'     field names.
#'
#' @return input parameter `x` sorted into the correct order.
#' 
#' @author Johannes Rainer
#' 
#' @export
mtd_sort <- function(x) {
    ordr <- rep(NA_integer_, nrow(x)) # NA will be last
    for (i in seq_along(.MTD_FIELD_ORDER)) {
        idx <- grep(paste0("^", .MTD_FIELD_ORDER[i]), x[, 1L])
        if (length(idx))
            ordr[idx] <- i
    }
    x[order(ordr), , drop = FALSE]
}
