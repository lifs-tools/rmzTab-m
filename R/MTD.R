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
#' for details and more information, in particular on the format of the mzTab-M
#' and on mandatory or optional fields.
#'
#' Generally, MTD data can be categarized into the following parts:
#' 
#' - *Core information*: general information on the experiment. A minimal
#'   set can be created using the [mtd_skeleton()] function, which might be
#'   further expanded with additional fields. This section allows to describe
#'   the general experimental setup. Also, it should contain references to
#'   **all** controlled vocabulary (CV) ontologies used and refered to in the
#'   mzTab-M file.
#'
#' - *Sample information*: optional information on individual samples that were
#'   measured with the various *assays*/*runs*. The [mtd_sample()] function
#'   assists in compiling the information for this section.
#' 
#' - *MS run information*: information on the individual MS *runs*
#'   (measurements of the samples). Each data file is one run. Use the
#'   [mtd_ms_run()] function to define this part of the metadata section.
#' 
#' - *Assay information*: the [mtd_assay()] function assists in compiling the
#'   assay section of the metadata. Mandatory fields are the name (ID) of the
#'   assay and the reference to the *MS run* in which the assay was measured.
#'   Optional information on sample reference, external links or custom
#'   information can be provided too. In most cases (except multiplexed assays
#'   or pre-fractionated samples) one assay will link to one MS run. Each assay
#'   **must** represent one column in the following *abundance matrix* sections.
#' 
#' - *Study variable information*: the [mtd_study_variables()] function allows
#'   to format study variable information from an experiment into the mzTab-M
#'   format. All study variables need to be assigned to at least one assay and
#'   must also be reported in the subsequent abundance matrices.
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
#' @note
#'
#' The general relationship between *ms_run*, *assay* and *sample*:
#' 
#' - one *ms_run* is the measurement of one assay.
#' 
#' - one assay can be measured by several MS runs (if fractionated) or multiple
#'   assays can be measured in the same MS run (if multiplexed).
#'
#' - one assay is (generally) one sample, but the same sample can be measured
#'   with multiple assays.
#' 
#' @author Philippine Louail, Johannes Rainer
#'
#' @examples
#'
#' ## Building the mzTab-M metadata information from a `data.frame` with sample
#' ## information of an experiment. Each row in that `data.frame` is one
#' ## measurement of one sample (i.e., represents one *ms_run*). Columns in
#' ## that `data.frame` provide the phenotypic and experimental variables of
#' ## each sample. The example below represents a simple experiment in which
#' ## 3 samples (e.g. cell lines) were measured, each at two different time
#' ## points (0 and 6 hours). In addition, one sample has the genotype *WT* and
#' ## two *KO*. Column `"operator"` contains the initials of the researcher
#' ## extracting the samples
#' exp <- data.frame(
#'     sample_name = c("S1_T1", "S1_T2", "S2_T1", "S2_T2", "S3_T1", "S3_T2"),
#'     sample_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
#'     timepoint = c("0h", "6h", "0h", "6h", "0h", "6h"),
#'     genotype = c("WT", "WT", "KO", "KO", "KO", "KO"),
#'     operator = c("BB", "BB", "BB", "BB", "FB", "FB"),
#'     file_name = c("s1-t1.mzML", "s1-t2.mzML", "s2-t1.mzML", "s2-t2.mzML",
#'                   "s3-t1.mzML", "s3-t2.mzML")
#' )
#' exp
#'
#' 
#' #############################################################################
#' ## Core metadata information
#'
#' ## We first compile the general metadata information. For the present
#' ## example we assume that we performed only preprocessing of the raw MS
#' ## data using *xcms*, thus we don't specify annotation databases used for
#' ## the compound identification/annotation. These could be provided through
#' ## the `database*` parameters. Also, the quantification method and unit(s)
#' ## could be specified using respective parameters of the function.
#' mtd <- mtd_skeleton(
#'     id = "EXP_001",
#'     software = "[MS, MS:1001582], xcms, 4.0.0")
#' mtd
#'
#' ## We next manually add a title and description for the experiment.
#' mtd <- rbind(
#'     mtd,
#'     c("title", "Experiment 1 preprocessed data"),
#'     c("description", "The preprocessed data of the experiment 1 samples."))
#'
#' ## We also add information on the MS instrumentation used
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
#' ## Other information, such as employed sample processing methods could be
#' ## added in a similar way.
#'
#' 
#' #############################################################################
#' ## Sample information
#'
#' ## We next add sample information to the metadata. In addition to the
#' ## specific sample properties that can be defined using the function's
#' ## parameters, arbitrary custom fields can be defined too. Below we add
#' ## information on sample extraction as custom information.
#' mtd_s <- mtd_sample(
#'     sample = unique(exp$sample_id),
#'     species = "[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]",
#'     tissue = "[BTO, BTO:0000759, liver, ]",
#'     cell_type = "[CL, CL:0000182, hepatocyte, ]",
#'     c("[,,Extraction date, 2011-12-21]",
#'       "[,,Extraction date, 2011-12-22]",
#'       "[,,Extraction date, 2011-12-23]")
#'     )
#' mtd_s
#'
#' mtd <- rbind(mtd, mtd_s)
#'
#' 
#' #############################################################################
#' ## MS run information
#'
#' ## The MS run information should capture information of each individual
#' ## measurement run on an MS instrument. For this, the original data file
#' ## names and location should be provided as well as the format of the
#' ## data files as well as polarity etc.
#' mtd_msr <- mtd_ms_run(
#'     location = exp$file_name,
#'     format = "[MS, MS:1000584, mzML file, ]",
#'     id_format = "[MS, MS:1000530, mzML unique identifier, ]",
#'     scan_polarity = "positive")
#'
#' mtd <- rbind(mtd, mtd_msr)
#'
#' 
#' #############################################################################
#' ## Assay information
#'
#' ## Each measurement should be associated to (at least) one assay. For our
#' ## simple example, each row in the `data.frame` represents one assay, with
#' ## each assay being measured in one MS run.
#' a <- mtd_assay(
#'     assay = exp$sample_name,
#'     sample_ref = c("sample[1]", "sample[1]", "sample[2]", "sample[2]",
#'                    "sample[3]", "sample[3]"),
#'     ms_run_ref = paste0("ms_run[", seq_len(nrow(exp)), "]")
#' )
#' a
#'
#' mtd <- rbind(mtd, a)
#'
#'
#' #############################################################################
#' ## Study variable information
#'
#' ## Study variables can be defined directly from the experiment `data.frame`.
#' ## In our example we use the columns (information on) `"timepoint"`,
#' ## `"genotype"` and `"operator"`. Importantly, the order of the provided
#' ## `data.frame` has to match the order of the assays (and MS runs).
#' svar <- mtd_study_variables(
#'     exp,
#'     study_variable_columns = c("timepoint", "genotype", "operator"))
#' svar
#'
#' mtd <- rbind(mtd, svar)
#'
#' ## Finally, the `mtd_sort()` function can be used to sort the generated
#' ## two-column matrix in the expected order.
#' mtd <- mtd_sort(mtd)
#'
#' ## This metadata information can next be exported manually, or using the
#' ## dedicated export helper functions to an mzTab-M file.
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
#' @seealso [MTD-export] for other functions defining metadata information
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
    res[order(res[, 3L]), 1:2, drop = FALSE]
}

#' @title Create a skeleton MTD section with general information
#'
#' @description
#'
#' This *core* MTD section allows to describe the general experimental setup
#' and provides general information of the data set. It should contain
#' references to **all** controlled vocabulary (CV) ontologies used and
#' refered to in the mzTab-M file.
#' The `mtd_skeleton()` function creates a two-column `matrix` with the
#' basic mzTab-M *MTD* section based on the provided data. The returned
#' result contains only minimal information. It should be expanded, corrected
#' and completed with additional fields and information (i.e., the
#' *skeleton* returned by this function should be completed with *flesh*).
#'
#' For details and expected input for the various parameter it is **strongly
#' suggested** to consult the [mzTab-M](https://github.com/HUPO-PSI/mzTab-M/blob/main/specification_documents/mzTab_format_specification_2_1-M.adoc#62-metadata-section) documentation.
#'
#' @param id `character(1)` (**mandatory**) with the ID of the data set.
#'
#' @param software `character` (**mandatory**) with the software(s) used.
#'     Can be of length > 1 if multiple softwares were used. Software should
#'     be provided in the order in which they were used.
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
#' @seealso [MTD-export] for other functions defining metadata information
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

#' @title msTab-M *sample* metadata information
#'
#' @description
#'
#' The `mtd_samples()` function aids in creating and formatting the (optional)
#' sample information from the mzTab-M metadata section. If defined, the sample
#' information **must** be correctly linked to from the *assay* section. In
#' particular, the assays need to link to the index of the samples defined in
#' this section. One entry for each originating sample should be defined
#' (without information on experimental properties). For each sample one
#' or more additional characteristics (such as `species`, `tissue`, `cell_type`
#' or `disease`) can be provided. Thus, these parameters expect the input be
#' provided as a `list`. In addition, if a single value needs to be assigned
#' to each sample, a `character(1)` of length 1 can be provided with the
#' respective input parameter.
#'
#' **Important:** to support the optional additional parameters passed along
#' with `...` **all** parameters (such as `sample`, `species` etc) have to
#' be **fully** spelled out.
#' 
#' For details and expected input for the various parameter it is **strongly
#' suggested** to consult the [mzTab-M](https://github.com/HUPO-PSI/mzTab-M/blob/main/specification_documents/mzTab_format_specification_2_1-M.adoc#62-metadata-section) documentation.
#'
#' @param ... optional *custom* information for each individual sample. Each
#'     custom variable is expected to be provided as a `character` of length
#'     equal to the length of parameter `sample`.
#' 
#' @param sample `character` with the labels/names of the individual samples.
#'
#' @param species `list` of length equal to `length(sample)` with each element
#'     providing the species (eventually multiple) for each sample. Can also
#'     be a `character` of `length(sample)` to assing a single species to each
#'     `sample`, or a `character(1)` of length one to assign the same species
#'     to every sample.
#'
#' @param tissue `list` with the tissue(s) of each sample. The same format
#'     as described for parameter `species` can be used.
#'
#' @param cell_type `list` with the cell type(s) of each sample. The same format
#'     as described for parameter `species` can be used.
#'
#' @param disease `list` with the disease(s) of each sample. The same format
#'     as described for parameter `species` can be used.
#'
#' @param description `character` of length equal to `length(sample)` with
#'     optional description of each sample.
#'
#' @return two column `character` `matrix` with the information formatted as
#'     sample section of the mzTab-M format.
#'
#' @seealso [MTD-export] for other functions defining metadata information
#' 
#' @export
#' 
#' @examples
#'
#' ## Example sample description data.frame for an experiment
#' pd <- data.frame(
#'     sample_name = c("ind_1", "ind_2", "ind_1", "ind_2"),
#'     sample_id = c("i1_t1", "i2_t2", "i1_t2", "i2_t2"),
#'     time_point = c(1, 2, 1, 2))
#'
#' ## Define a minimal sample information with just the sample names.
#' mtd_sample(unique(pd$sample_name))
#'
#' ## Add also species information: each sample from the same species
#' mtd_sample(
#'     sample = unique(pd$sample_name),
#'     species = "[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]")
#'
#' ## Assume first sample is a mixture of two species
#' mtd_sample(
#'     sample = unique(pd$sample_name),
#'     species = list(c("[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]",
#'                      "[NCBITaxon, NCBITaxon:39767, Human rhinovirus 11, ]"),
#'                    "[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]")
#' )
#'
#' ## Add full information including tissue, cell type and disease
#' mtd_sample(
#'     sample = unique(pd$sample_name),
#'     species = list(c("[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]",
#'                      "[NCBITaxon, NCBITaxon:39767, Human rhinovirus 11, ]"),
#'                    "[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]"),
#'     tissue = "[BTO, BTO:0000759, liver, ]",
#'     cell_type = "[CL, CL:0000182, hepatocyte, ]",
#'     disease = list(c("[DOID, DOID:684, hepatocellular carcinoma, ]",
#'                      "[DOID, DOID:9451, alcoholic fatty liver, ]"),
#'                    NULL)
#' )
#'
#' ## Add also additional custom variables
#' mtd_sample(sample = c("A", "B"),
#'     c("[,,Extraction date, 2011-12-21]",
#'       "[,,Extraction date, 2011-12-22]"),
#'     c("[,,Extraction reason, liver biopsy]",
#'       "[,,Extraction reason, liver biopsy]"))
mtd_sample <- function(..., sample = character(), species = list(),
                       tissue = list(), cell_type = list(), disease = list(),
                       description = character()) {
    l <- length(sample)
    if (!l)
        return(matrix(ncol = 2, nrow = 0, NA_character_))
    s <- seq_len(l)
    res <- cbind(mtd_fields(sample, field_prefix = "sample"), order = s)
    if (length(species)) {
        if (length(species) != l)
            species <- rep(species[1], l)
        if (!is.list(species)) species <- as.list(species)
        res <- rbind(res, .mtd_multi_fields(species, "sample", "species"))
    }
    if (length(tissue)) {
        if (length(tissue) != l)
            tissue <- rep(tissue[1], l)
        if (!is.list(tissue)) tissue <- as.list(tissue)
        res <- rbind(res, .mtd_multi_fields(tissue, "sample", "tissue"))
    }
    if (length(cell_type)) {
        if (length(cell_type) != l)
            cell_type <- rep(cell_type[1], l)
        if (!is.list(cell_type)) cell_type <- as.list(cell_type)
        res <- rbind(res, .mtd_multi_fields(cell_type, "sample", "cell_type"))
    }
    if (length(disease)) {
        if (length(disease) != l)
            disease <- rep(disease[1], l)
        if (!is.list(disease)) disease <- as.list(disease)
        res <- rbind(res, .mtd_multi_fields(disease, "sample", "disease"))
    }
    if (length(description)) {
        if (length(description) != l)
            stop("If provided, 'description' has to be of length equal to the",
                 " number of samples", call. = FALSE)
        res <- rbind(res,
                     cbind(mtd_fields(description = description,
                                      field_prefix = "sample"),
                           s))
    }
    ## optional fields ("custom") passed through `...`
    res <- rbind(res, .mtd_custom_fields(..., expected_length = l))
    res[order(res[, 3L]), 1:2, drop = FALSE]
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
#'     mzML format, `format = "[MS, MS:1000530, mzML unique identifier, ]"`
#'     can be used.
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
#' @seealso [MTD-export] for other functions defining metadata information
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

#' @title mzTab-M *assay* metadata information
#'
#' @description
#'
#' The `mtd_assay()` function assists in compiling the *assay* information of
#' the metadata section. Each assay **must** be associated with at least one
#' entry of the *ms_run* section (see [mtd_ms_run()]). This mapping can be
#' defined with the `ms_run_ref` parameter by providing the ID/name of the
#' run (e.g. `"ms_run[1]"`).
#'
#' **Important:** to support the optional additional parameters passed along
#' with `...` **all** parameters (such as `assay`, `sample_ref` etc) have to
#' be **fully** spelled out.
#'
#' For details and expected input for the various parameter it is **strongly
#' suggested** to consult the [mzTab-M](https://github.com/HUPO-PSI/mzTab-M/blob/main/specification_documents/mzTab_format_specification_2_1-M.adoc#62-metadata-section) documentation.
#'
#' @param ... optional additional (custom) parameters for each assay. If
#'     provided, the length of the `character` vector(s) have to match the
#'     length of parameter `assay`.
#' 
#' @param assay `character` with the names of the assay(s). Each assay **must**
#'     be reported in the following sections (e.g. the SMF section).
#'
#' @param exterinal_uri optional `character` with a reference to further
#'     information about the assay, for example via a reference to an object
#'     within an ISA-TAB file. Can be of length 1 (in which case the same
#'     reference is assigned to every assay) or length equal to the length
#'     of `assay`.
#'
#' @param sample_ref optional `character` with the ID/name of the sample for
#'     the assay (e.g. `"sample[1]"`). If provided, its length has to match
#'     the length of `assay`.
#'
#' @param ms_run_ref `character` with the ID of associated *ms_run*(s). For
#'     multiplexed assays, different assays can refer to the same run. To
#'     support pre-fractionated samples, it is also possible to provide a
#'     `list` of `character` with the runs the assay was measured in. See
#'     examples below for more details.
#'
#' @return two-column `character` `matrix` with the content for the assay
#'     metadata section.
#' 
#' @author Johannes Rainer
#'
#' @seealso [MTD-export] for other functions defining metadata information
#' 
#' @export
#'
#' @examples
#'
#' ## Minimal example with assay and ms_run_ref defined, each assay assigned
#' ## to its own MS run. Note that for **all** parameters the **full**
#' ## parameter name has to be used (e.g., `assay = `).
#' mtd_assay(assay = c("a1", "a2", "a3"),
#'     ms_run_ref = c("ms_run[1]", "ms_run[2]", "ms_run[3]"))
#'
#' ## Example for a multiplexed assay.
#' mtd_assay(assay = c("a1", "a2", "a3"),
#'     ms_run_ref = c("ms_run[1]", "ms_run[1]", "ms_run[1]"))
#'
#' ## Example for a pre-fractionated samples
#' mtd_assay(assay = c("a1", "a2", "a3"),
#'     ms_run_ref = list(c("ms_run[1]", "ms_run[2]"),
#'                       c("ms_run[3]", "ms_run[4]"),
#'                       c("ms_run[5]", "ms_run[6]")))
#' 
#' ## Example adding also sample reference and an external_uri
#' mtd_assay(
#'     assay = c("a1", "a2", "a3"),
#'     external_uri = "https://www.ebi.ac.uk/metabolights/MTBLS517/files/i_Investigation.txt",
#'     sample_ref = c("sample[1]", "sample[1]", "sample[2]"),
#'     ms_run_ref = c("ms_run[1]", "ms_run[2]", "ms_run[3]"))
#'
#' ## Providing additional, custom information for each assay. These can be
#' ## passed as `character` vectors (same length than `assay`!).
#' mtd_assay(assay = c("a1", "a2", "a3"),
#'     ms_run_ref = c("ms_run[1]", "ms_run[2]", "ms_run[3]"),
#'     c("[MS, , Assay operator, Fred Blogs]",
#'       "[MS, , Assay operator, Fred Blogs]",
#'       "[MS, , Assay operator, Frodo]"))
mtd_assay <- function(..., assay = character(), external_uri = character(),
                      sample_ref = character(), ms_run_ref = character()) {
    l <- length(assay)
    if (!length(assay))
        return(matrix(ncol = 2, nrow = 0, NA_character_))
    if (!length(ms_run_ref))
        stop("Parameter 'ms_run_ref' is required", call. = FALSE)
    if (l != length(ms_run_ref))
        stop("lengths of parameters 'assay' and 'ms_run_ref' have to match",
             call. = FALSE)
    s <- seq_len(l)
    res <- cbind(mtd_fields(assay, field_prefix = "assay"), order = s)
    if (length(external_uri)) {
        if (length(external_uri) != l) external_uri <- rep(external_uri[1L], l)
        res <- rbind(res, cbind(mtd_fields(external_uri = external_uri,
                                           field_prefix = "assay"), s))
    }
    if (length(sample_ref)) {
        if (length(sample_ref) != l)
            stop("Length of 'sample_ref' has to match the length of 'assay'",
                 call. = FALSE)
        res <- rbind(res, cbind(mtd_fields(sample_ref = sample_ref,
                                           field_prefix = "assay"), s))
    }
    if (is.list(ms_run_ref)) {
        ls <- lengths(ms_run_ref)
        if (any(ls < 1))
            stop("At least one ms_run reference must be defined for each ",
                 "assay", call. = FALSE)
        res <- rbind(res, cbind(.mtd_multi_fields(
                              ms_run_ref, prefix = "assay",
                              suffix = "ms_run_ref")))
    } else
        res <- rbind(res, cbind(mtd_fields(ms_run_ref = ms_run_ref,
                                           field_prefix = "assay"), s))
    ## Optional "custom" fields passed through ...
    res <- rbind(
        res, .mtd_custom_fields(..., expected_length = l, prefix = "assay"))
    res[order(res[, 3L]), 1:2, drop = FALSE]    
}

#' @title mzTab-M *study variables* metadata information
#'
#' @description
#'
#' A *study variable* in the mzTab-M definition is the **value** of an
#' experimental variable (condition or factor), e.g., `"control"`, or
#' `"2 hours"` (i.e., it is not the name of the factor). Each study variable
#' **must** be reported in the following abundance tables. Each assay of a data
#' set must be referred to from at least one study variable* Even if a data set
#' has no experimental variables, a study variable **must** be reported,
#' linking to all assays, with the name `"undefined"`.
#' 
#' Information for the mzTab-M *study variable* metadata section can be created
#' using the `mtd_study_variables()` function. This function extracts the
#' relevant information from an input `data.frame` in which rows are expected
#' to represent *assays* (each row being one assay) and columns the experimental
#' factors that should be added as study variables.
#'
#' The `mtd_define_study_variables()` function can be used to get the set
#' (and order) of study variables that would be generated from an input
#' `data.frame` depending on the parameter `study_variable_columns`.
#'
#' @param x `data.frame` with rows corresponding to individual *assays* and
#'     columns containing the experimental conditions/study variables. The
#'     number of rows is thus expected to be the same as the number of assays
#'     defined in the *assay* metadata section (using e.g., [mtd_assay()]) and
#'     the order of rows is expected to match the order of assays.
#'
#' @param study_variable_columns `character` with the names of the columns in
#'     `x`to be included as study variables. If not defined (the default) a
#'     single study variable will be defined assigning all assays to it.
#'
#' @param average_function optional `character` defining the function used to
#'     calculate the study variable quantification value (reported in the
#'     following table(s)). Can be of length 1 or equal to the number of study
#'     variables (to allow defining a different function per variable). Use
#'     `mtd_define_study_variables()` to get the complete set of study
#'     variables for parameters `x` and `study_variable_columns`. Defaults
#'     to the arithmetic mean.
#'
#' @param variation_function optional `character` defining the function used to
#'     calculate the study variable quantification variation value (reported in
#'     the following table(s)). Can be of length 1 or equal to the number of
#'     study variables (to allow defining a different function per variable).
#'     Use `mtd_define_study_variables()` to get the complete set of study
#'     variables for parameters `x` and `study_variable_columns`. Defaults
#'     to the coefficient of variation.
#'
#' @param description `character` with a textual description of the study
#'     variable. If provided, its length needs to be equal to the number of
#'     study variables. Use `mtd_define_study_variables()` to get the complete
#'     set of study variables for parameters `x` and `study_variable_columns`.
#'     If not provided (the default) the description is used combines the
#'     column name in `x` and the value of the variable.
#'
#' @param factors Currently not supported.
#' 
#' @return two-column `character` `matrix` with the content for the study
#'     variables metadata section.
#'
#' @author Philippine Louail, Johannes Rainer
#'
#' @seealso [MTD-export] for other functions defining metadata information
#' 
#' @export
#'
#' @examples
#'
#' ## Example phenodata/sample data.frame. Each row is supposed to match
#' ## the measurement of one sample (for a certain condition/time point) from
#' ## one individual 
#' x <- data.frame(
#'     name = c("I1_0", "I2_0", "I1_6", "I2_6", "I3_0"),
#'     individual = c("I1", "I2", "I1", "I2", "I3"),
#'     timepoint = c("0h", "6h", "0h", "6h", "0h"),
#'     T2D = c(TRUE, FALSE, TRUE, FALSE, FALSE)
#' )
#'
#' ## Study variables for this data set would be `"timepoint"` and `"T2D"`:
#' mtd_study_variables(x, study_variable_columns = c("timepoint", "T2D"))
#' 
#' ## Specifying a different average and variation function
#' mtd_study_variables(x,
#'     study_variable_columns = c("timepoint", "T2D"),
#'     average_function = "[MS, MS:1002883, median, ]",
#'     variation_function = "[MS, MS:1002885, standard error, ]")
#'
#' ## Creating a study variable section without defined study variables
#' mtd_study_variables(x)
mtd_study_variables <- function(x, study_variable_columns = character(),
                                average_function = "[MS, MS:1002962, mean, ]",
                                variation_function = "[MS, MS:1002963, variation coefficient, ]",
                                description = character(),
                                factors = character()) {
    if (length(factors))
        stop("'factors' is currently not supported", call. = FALSE)
    if (length(study_variable_columns)) {
        if (!all(study_variable_columns %in% colnames(x)))
            stop("Not all column names defined with 'study_variable_columns' ",
                 "present in 'x'", call. = FALSE)
        svar_m <- .mztab_study_variables(x, study_variable_columns)
        svars <- unique(as.vector(svar_m))
    } else {
        ## Define a single study variable and assign all assays to it.
        svars <- "undefined"
        svar_m <- matrix(ncol = 1, nrow = nrow(x), svars)
        description <- "Undefined"
    }
    l <- length(svars)
    if (!length(description)) {
        description <- vapply(svars, function(z) {
            z <- strsplit(z, split = ":")[[1L]]
            paste0("Column: ", z[1L], ", value: ", z[2L])
        }, NA_character_)
    }
    if (length(average_function) == 1L)
        average_function <- rep(average_function, l)
    if (length(variation_function) == 1L)
        variation_function <- rep(variation_function, l)
    if (length(average_function)  != l)
        stop("Length of parameter 'average_function' has to be equal to ",
             "the number of study variables", call. = FALSE)
    if (length(variation_function)  != l)
        stop("Length of parameter 'variation_function' has to be equal to ",
             "the number of study variables", call. = FALSE)
    if (length(description)  != l)
        stop("Length of parameter 'description' has to be equal to ",
             "the number of study variables", call. = FALSE)
    ## Build matrix
    res <- matrix(ncol = 2, nrow = 0, NA_character_)
    for (i in seq_along(svars)) {
        idx <- which(svar_m == svars[i], arr.ind = TRUE)
        res <- rbind(
            res,
            matrix(ncol = 2,
                   c(paste0("study_variable[", i, "]"),
                     paste0("study_variable[", i, "]-assay_refs"),
                     paste0("study_variable[", i, "]-average_function"),
                     paste0("study_variable[", i, "]-variation_function"),
                     paste0("study_variable[", i, "]-description"),
                     paste0(svars[i]),
                     paste0("assay[", idx[, "row"], "]", collapse = "|"),
                     average_function[i],
                     variation_function[i],
                     description[i])))
    }
    res
}

#' @rdname mtd_study_variables
#'
#' @export
mtd_define_study_variables <- function(x, study_variable_columns = character()){
    unique(as.vector(.mztab_study_variables(x, study_variable_columns)))
}

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
#' @seealso [MTD-export] for other functions defining metadata information
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

################################################################################
##    INTERNAL HELPER FUNCTIONS
################################################################################

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

#' Helper function to create xxx[1]-custom[1] fields for `character` vectors
#' passed through `...`. It is somewhat similar to the `.mtd_multi_fields()`,
#' but takes arbitrary many input variables, that have however all to have
#' the same length. Thus, it is expected that for each element exactly one
#' value is provided, while `mtd_multi_fields()` supports a variable number
#' of values per element (and hence requires a `list` as input).
#'
#' @return `character` `matrix` with 3 columns (third column being the index)
#'     that could be used to order the rows later.
#'
#' @author Johannes Rainer
#' 
#' @noRd
#'
#' @examples
#'
#' ## Single custom field
#' .mtd_custom_fields(c("a", "b", "c"), prefix = "sample", expected_length = 3)
#'
#' ## Two custom fields per sample
#' .mtd_custom_fields(c("a", "b", "c"), c("A", "B", "C"),
#'     prefix = "sample", expected_length = 3)
.mtd_custom_fields <- function(..., prefix = "sample", suffix = "custom",
                               expected_length = 0L) {
    dots <- list(...)
    message(length(dots))
    s <- seq_len(expected_length)
    if (length(dots)) {
        do.call(rbind, lapply(seq_along(dots), function(i) {
            z <- dots[[i]]
            if (length(z) != expected_length)
                stop("If optional custom information is provided the length ",
                     "has to match the length of '", prefix, "'", call. = FALSE)
            cbind(paste0(prefix, "[", s, "]-", suffix, "[", i, "]"), z, s)
        }))
    } else matrix(ncol = 3, nrow = 0, NA_character_)
}

#' Helper function to create entries in the format `<prefix>[i]-<suffix>[j]`,
#' e.g. `"sample[1]-species[1]"` `"sample[1]-species[2]"`, i.e., fields with
#' multiple possible values. The information **must** be passed as a `list`,
#' each list element providing the information for one entity. A list element
#' can also be `NULL` if for one entity no value is defined.
#'
#' See also the `.mtd_custom_fields()` function for an alternative.
#'
#' @param x `list` with values per field `prefix`. The length of `x` defines
#'     the index `i`, while the length of each list element defines the index
#'     `j`.
#'
#' @param prefix `character(1)` with the suffix of the field.
#'
#' @param suffix `character(1)` with the name of the field.
#'
#' @return 3 column `character` `matrix`, the third column being the index along
#'     `x` which can be used to re-order the results.
#'
#' @noRd
#'
#' @author Johannes Rainer
#'
#' @examples
#'
#' ## 1:1 sample to species mapping, with no species defined for the 2nd sample
#' .mtd_multi_fields(
#'     list("[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]",
#'          NULL,
#'          "[NCBITaxon, NCBITaxon:39767, Human rhinovirus 11, ]"),
#'     prefix = "sample", suffix = "species")
#'
#' ## 2 species for the first sample, none for the second and one for the third
#' .mtd_multi_fields(
#'     list(c("[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]",
#'            "[NCBITaxon, NCBITaxon:39767, Human rhinovirus 11, ]"),
#'          NULL,
#'          "[NCBITaxon, NCBITaxon:39767, Human rhinovirus 11, ]"),
#'     prefix = "sample", suffix = "species")
.mtd_multi_fields <- function(x, prefix, suffix) {
    do.call(rbind, lapply(seq_along(x), function(i) {
        vals <- x[[i]]
        if (lv <- length(vals)) {
            cbind(paste0(prefix, "[", rep(i, lv), "]-",
                         suffix, "[", seq_len(lv), "]"),
                  vals,
                  order = .prefix_zero(rep(i, lv)))
        }
    }))
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

#' @description
#'
#' Creates a `matrix` of study variables.
#'
#' @param x `data.frame` with sample annotations. Each row being one assay.
#'
#' @param study_variable_columns `character` with the names of the column(s)
#'     from which study variables should be defined.
#'
#' @return `character` `matrix`, same number of rows as `x` with the study
#'     variables in columns. Each cell represents the study variable value
#'     allowing to assign a specific sample (row) to the respective study
#'     variable.
#'
#' @noRd
#'
#' @author Philippine Louail
#' 
#' @examples
#'
#' x <- data.frame(sex = c("male", "female", "female", "male", "male"),
#'                 group = c("case", "case", "control", "case", "control"))
#'
#' .mztab_study_variables(x, c("sex", "group"))
#'
#' .mztab_study_variables(x, "sex")
.mztab_study_variables <- function(x = data.frame(),
                                   study_variable_columns = colnames(x), 
                                   sep = ":") {
    do.call(cbind, lapply(study_variable_columns,
                          function(z) paste0(z, sep, x[, z])))
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
