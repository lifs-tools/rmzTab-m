## Code related to import/export of the MTD element

#' @title Defining and exporting the mzTab-M metadata table
#'
#' @name MTD-export
#' 
#' @description
#'
#' The metadata section/table of the mzTab-M definition is comprehensive, but
#' also tricky to define. The *rmzTabM* package provides a variety of utility
#' functions that help defining this information. These might be re-used for
#' software package developers to export metabolomics results from their
#' respective software. Importantly, the helper functions listed here only
#' define the core elements for the MTD section, helping with re-arranging and
#' reformatting information available e.g. in `data.frame` format into the
#' respective fields in the MTD section. Additional (optional) fields might
#' need to be added manually depending on availability for an experiment.
#'
#' See also the [specification of the MTD section](https://github.com/HUPO-PSI/mzTab-M/blob/main/specification_documents/mzTab_format_specification_2_1-M.adoc#62-metadata-section)
#' for details and more information.
#'
#' Generally, MTD data can be categarized into the following parts:
#' 
#' - *Core information*: general information on the experiment. A minimal
#'   set can be created using the [mtb_skeleton()] function, which might be
#'   further expanded with additional fields.
#'
#' - *Sample information*:
#' 
#' - *MS run information*: information on the individual MS *runs*
#'   (measurements of the samples). Each data file is one run. Use the
#'   [mtd_ms_run()] function to define this part of the metadata section.
#' 
#' - *Assay information*:
#' 
#' - *Study variable information*:
#'
#'
#' The helper function listed above can be used sequentially to create the
#' metadata information. See the examples below for a general approach how to
#' define the MTD section of an experiment.
#'
#' In addition, various helper functions are available to assist in MTD data
#' generation:
#'
#' - [mtd_sort()]: to sort the MTD `matrix` into the expected order.
#' - [mtd_fields()]: helps formatting values into the mzTab-M-specific format.
#'
#' @author Johannes Rainer, Philippine Louail
NULL


#' @title Prepare and format information for the mzTab-M metadata section
#'
#' @description
#'
#' This function assists in creating and formatting information for the
#' mzTab-M metadata section (MTD). It combines and formats the provided input
#' values for a specific field.
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

#' @title Create a skeleton MTD section with general information
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
                         cv_full_name = c("PSI-MS controlled vocabulary",
                                          "PRIDE PRoteomics IDEntifications (PRIDE) database controlled vocabulary"),
                         cv_version = c("4.1.138", "16:10:2023 11:38"),
                         cv_uri = c("https://raw.githubusercontent.com/HUPO-PSI/psi-ms-CV/master/psi-ms.obo",
                                    "https://www.ebi.ac.uk/ols/ontologies/pride"),
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

#' @title msTab-M *ms_run* metadata fields
#'
#' @description
#'
#' The `mtd_ms_run()` function allows to define and format the *ms_run* fields
#' of the mzTab-M metadata. The information is build on the actual data file
#' names along with optional additional parameters to characterize the MS
#' run(s).
#' 
#' For details and expected input for the various parameter it is **strongly
#' suggested** to consult the [mzTab-M](https://github.com/HUPO-PSI/mzTab-M/blob/main/specification_documents/mzTab_format_specification_2_1-M.adoc#62-metadata-section) documentation.
#' 
#' @param location `character` with the location (and file name) of the
#'     individual runs. Each element will be one run. This parameter is
#'     required, set to `"null"` if the location of the file(s) is not known.
#'
#' @param instrument_ref (optional) `integer()` with the index of the instrument
#'     the run was measured on.
#'
#' @param format (optional) `character` defining the format of the external MS
#'     data file. If specified, also `id_format` has be be provided. Can be of
#'     length 1 or equal to `length(location)`. For data file(s) in mzML format,
#'     `format = "[MS, MS:1000584, mzML file, ]"` can be used.
#'
#' @param id_format (optional) `character` defining the id format used in the
#'     external data file. If specified, also `format` needs to be defined.
#'     Can be of length 1 or equal to `length(location)`. For data file(s) in
#'     mzML format, `format = "[MS, MS:1000584, mzML file, ]"` can be used.
#'
#' @param fragmentation_method (optional) `list` of `character` defining the
#'     type(s) of fragmentation(s) used in a given ms run. Length must match
#'     length of `location` if provided. If no fragmentation was used for a
#'     specific file/run use `NULL` for that `list` element (position). As
#'     example, if two runs are included, the first does not have any
#'     fragmentation and for the second CID and HCD was used define
#'     `list(NULL, c("[MS, MS:1000133, CID, ]", "[MS, MS:1000422, HCD, ]"))`.
#'
#' @param scan_polarity `character` defining the polarity of a run. Can be
#'     either `"positive"` or `"negative"`. Can be of length 1 or equal to
#'     `length(location)`.
#'
#' @param hash (optional) `character` with the hash value of the corresponding
#'     external MS data file. If provided, also `hash_method` needs to be
#'     defined. The length of `hash` has to match the length of `location`.
#'
#' @param hash_method (optional) `character` with the hash method used to
#'     generate the value in `hash`. If provided, also `hash` needs to be
#'     defined. The length of `hash_method` has to match the length of `hash`.
#' 
#' @note
#'
#' At present only a single polarity per run/file is supported.
#'
#' @return two column `character` `matrix` with the *ms_run* metadata fields
#'     for a mzTab-M file.
#'
#' @author Johannes Rainer, Philippine Louail
#'
#' @export
#' 
#' @examples
#'
#' ## Build a very basic MTD ms_run section for two data files
#' fls <- c("file:///path/to/file/a.mzML", "file:///path/to/file/b.mzML")
#' mtd_ms_run(location = fls, scan_polarity = "positive")
#'
#' ## Add also instrument reference information
#' mtd_ms_run(location = fls, scan_polarity = "positive", instrument_ref = 1)
#'
#' ## Finally, add a fragmentation method used for the second file - no
#' ## fragmentation was used for the first file, thus `NULL` is specified.
#' ## Parameter `fragmentation_method` expects a `list` as input to support
#' ## also multiple fragmentation methods per MS run.
#' mtd_ms_run(location = fls, scan_polarity = "positive",
#'     fragmentation_method = list(NULL, "[MS, MS:1000133, CID, ]"))
mtd_ms_run <- function(location = character(),
                       instrument_ref = integer(),
                       format = character(),
                       id_format = character(),
                       fragmentation_method = vector("list", length(location)),
                       scan_polarity = character(),
                       hash = character(),
                       hash_method = character()) {
    l <- length(location)
    s <- seq_len(l)
    if (!l)
        stop("ms_run: parameter 'location' is required, even if it is \"null\"",
             call. = FALSE)
    if (!length(scan_polarity))
        stop("ms_run: parameter 'scan_polarity' is required", call. = FALSE)
    if ((length(format) | length(id_format)) &
        (length(format) != length(id_format)))
        stop("ms_run: either both 'format' and 'id_format' have to be ",
             "defined or none of the two.", call. = FALSE)
    if ((length(hash) | length(hash_method)) &
        (length(hash) != length(hash_method)))
        stop("ms_run: either both 'hash' and 'hash_method' have to be ",
             "defined or none of the two.", call. = FALSE)
    if (length(hash) && length(hash) != l)
        stop("ms_run: if provided, length of parameter 'hash' has to ",
             "match length of 'location'", call. = FALSE)
    if (length(fragmentation_method) != l)
        stop("ms_run: length of parameter 'fragment_method' has to match ",
             "length of 'location'", call. = FALSE)
    ## Build data        
    res <- .ms_run_format(s, "location", location)
    if (l2 <- length(instrument_ref)) {
        if (l2 != l) instrument_ref <- rep(instrument_ref[1L], l)
        res <- rbind(
            res, .ms_run_format(s, "instrument_ref",
                                paste0("instrument[", instrument_ref, "]")))
    }
    if (l2 <- length(format)) {
        if (l2 != l) format <- rep(format[1L], l)
        res <- rbind(res, .ms_run_format(s, "format", format))
    }
    if (l2 <- length(id_format)) {
        if (l2 != l) id_format <- rep(id_format[1L], l)
        res <- rbind(res, .ms_run_format(s, "id_format", id_format))
    }
    ## fragmentation_method
    frag_mod <- lapply(seq_along(fragmentation_method), function(z) {
        vals <- fragmentation_method[[z]]
        if (lv <- length(vals)) {
            cbind(paste0("ms_run[", rep(z, lv), "]-fragmentation_method[",
                         seq_len(lv), "]"),
                  fragmentation_method[[z]],
                  order = .prefix_zero(rep(z, lv)))
        }
    })
    frag_mod <- do.call(rbind, frag_mod)
    if (length(frag_mod)) res <- rbind(res, frag_mod)
    if (length(scan_polarity) != l) scan_polarity <- rep(scan_polarity[1L], l)
    res <- rbind(res, .ms_run_format(s, "scan_polarity[1]",
                                     .ms_scan_polarity(scan_polarity)))
    if (length(hash)) res <- rbind(res, .ms_run_format(s, "hash", hash))
    if (length(hash_method))
        res <- rbind(res, .ms_run_format(s, "hash_method", hash_method))
    res[order(res[, 3L]), 1:2, drop = FALSE]
}

#' @param x would be sequence from 1 to number of runs
#'
#' @param name the name of the field
#'
#' @param values the actual values
#'
#' @return 3 column `matrix`
#'
#' @noRd
#'
#' @examples
#'
#' .ms_run_format(1:3, "format", rep("[MS, MS:1000584, mzML file, ]", 3))
.ms_run_format <- function(x, name, values) {
    cbind(paste0("ms_run[", x, "]-", name),
          values, order = .prefix_zero(x))
}

#' Helper to convert `"positive"` and `"negative"` polarity into the respective
#' terms from the MS ontology.
#'
#' @noRd
.ms_scan_polarity <- function(x) {
    if (!all(x %in% c("positive", "negative")))
        stop("'scan_polarity' has to be either \"positive\" or ",
             "\"negative\".", call. = FALSE)
    x[x == "positive"] <- "[MS, MS:1000130, positive scan, ]"
    x[x == "negative"] <- "[MS, MS:1000129, negative scan, ]"
    x
}

.mtd_assay <- function() {
}

.mtd_study_variable <- function() {
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
