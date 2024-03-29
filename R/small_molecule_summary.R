# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@cebitec.uni-bielefeld.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title SmallMoleculeSummary
#' @description SmallMoleculeSummary Class
#' @format An \code{R6Class} generator object
#' @field prefix  character [optional]
#'
#' @field header_prefix  character [optional]
#'
#' @field sml_id  integer 
#'
#' @field smf_id_refs  list( integer ) [optional]
#'
#' @field database_identifier  list( character ) [optional]
#'
#' @field chemical_formula  list( character ) [optional]
#'
#' @field smiles  list( character ) [optional]
#'
#' @field inchi  list( character ) [optional]
#'
#' @field chemical_name  list( character ) [optional]
#'
#' @field uri  list( character ) [optional]
#'
#' @field theoretical_neutral_mass  list( numeric ) [optional]
#'
#' @field adduct_ions  list( character ) [optional]
#'
#' @field reliability  character [optional]
#'
#' @field best_id_confidence_measure  \link{Parameter} [optional]
#'
#' @field best_id_confidence_value  numeric [optional]
#'
#' @field abundance_assay  list( numeric ) [optional]
#'
#' @field abundance_study_variable  list( numeric ) [optional]
#'
#' @field abundance_variation_study_variable  list( numeric ) [optional]
#'
#' @field opt  list( \link{OptColumnMapping} ) [optional]
#'
#' @field comment  list( \link{Comment} ) [optional]
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SmallMoleculeSummary <- R6::R6Class(
  'SmallMoleculeSummary',
  public = list(
    `prefix` = NULL,
    `header_prefix` = NULL,
    `sml_id` = NULL,
    `smf_id_refs` = NULL,
    `database_identifier` = NULL,
    `chemical_formula` = NULL,
    `smiles` = NULL,
    `inchi` = NULL,
    `chemical_name` = NULL,
    `uri` = NULL,
    `theoretical_neutral_mass` = NULL,
    `adduct_ions` = NULL,
    `reliability` = NULL,
    `best_id_confidence_measure` = NULL,
    `best_id_confidence_value` = NULL,
    `abundance_assay` = NULL,
    `abundance_study_variable` = NULL,
    `abundance_variation_study_variable` = NULL,
    `opt` = NULL,
    `comment` = NULL,
    #' @description Create a new SmallMoleculeSummary.
    #' @param sml_id The small molecule summary id.
    #' @param prefix 'SML'.
    #' @param header_prefix 'SMH'.
    #' @param smf_id_refs References by id to \link{SmallMoleculeFeature}s.
    #' @param database_identifier The database identifiers.
    #' @param chemical_formula The chemical formulas.
    #' @param smiles The SMILES strings.
    #' @param inchi The INCHI identifiers.
    #' @param chemical_name The chemical names.
    #' @param uri External URIs.
    #' @param theoretical_neutral_mass The theoretical neutral masses.
    #' @param adduct_ions The adduct ions.
    #' @param reliability The reliability according to the system defined in the \link{Metadata} section.
    #' @param best_id_confidence_measure The best id confidence measure \link{Parameter}.
    #' @param best_id_confidence_value The best id confidence value.
    #' @param abundance_assay The abundances over all assays.
    #' @param abundance_study_variable The abundances over study variables. 
    #' @param abundance_variation_study_variable The abundances variation over study variables.
    #' @param opt Optional columns and values.
    #' @param comment Comments.
    #' @param ... local optional variable arguments.
    initialize = function(`sml_id`, `prefix`='SML', `header_prefix`='SMH', `smf_id_refs`=NULL, `database_identifier`=NULL, `chemical_formula`=NULL, `smiles`=NULL, `inchi`=NULL, `chemical_name`=NULL, `uri`=NULL, `theoretical_neutral_mass`=NULL, `adduct_ions`=NULL, `reliability`=NULL, `best_id_confidence_measure`=NULL, `best_id_confidence_value`=NULL, `abundance_assay`=NULL, `abundance_study_variable`=NULL, `abundance_variation_study_variable`=NULL, `opt`=NULL, `comment`=NULL, ...){
      local.optional.var <- list(...)
      if (!missing(`sml_id`)) {
        stopifnot(is.numeric(`sml_id`), length(`sml_id`) == 1)
        self$`sml_id` <- `sml_id`
      }
      if (!is.null(`prefix`)) {
        stopifnot(is.character(`prefix`), length(`prefix`) == 1)
        self$`prefix` <- `prefix`
      }
      if (!is.null(`header_prefix`)) {
        stopifnot(is.character(`header_prefix`), length(`header_prefix`) == 1)
        self$`header_prefix` <- `header_prefix`
      }
      if (!is.null(`smf_id_refs`)) {
        stopifnot(is.vector(`smf_id_refs`), length(`smf_id_refs`) != 0)
        sapply(`smf_id_refs`, function(x) stopifnot(is.numeric(x)))
        self$`smf_id_refs` <- `smf_id_refs`
      }
      if (!is.null(`database_identifier`)) {
        stopifnot(is.vector(`database_identifier`), length(`database_identifier`) != 0)
        sapply(`database_identifier`, function(x) stopifnot(is.character(x)))
        self$`database_identifier` <- `database_identifier`
      }
      if (!is.null(`chemical_formula`)) {
        stopifnot(is.vector(`chemical_formula`), length(`chemical_formula`) != 0)
        sapply(`chemical_formula`, function(x) stopifnot(is.character(x)))
        self$`chemical_formula` <- `chemical_formula`
      }
      if (!is.null(`smiles`)) {
        stopifnot(is.vector(`smiles`), length(`smiles`) != 0)
        sapply(`smiles`, function(x) stopifnot(is.character(x)))
        self$`smiles` <- `smiles`
      }
      if (!is.null(`inchi`)) {
        stopifnot(is.vector(`inchi`), length(`inchi`) != 0)
        sapply(`inchi`, function(x) stopifnot(is.character(x)))
        self$`inchi` <- `inchi`
      }
      if (!is.null(`chemical_name`)) {
        stopifnot(is.vector(`chemical_name`), length(`chemical_name`) != 0)
        sapply(`chemical_name`, function(x) stopifnot(is.character(x)))
        self$`chemical_name` <- `chemical_name`
      }
      if (!is.null(`uri`)) {
        stopifnot(is.vector(`uri`), length(`uri`) != 0)
        sapply(`uri`, function(x) stopifnot(is.character(x)))
        self$`uri` <- `uri`
      }
      if (!is.null(`theoretical_neutral_mass`)) {
        stopifnot(is.vector(`theoretical_neutral_mass`), length(`theoretical_neutral_mass`) != 0)
        sapply(`theoretical_neutral_mass`, function(x) stopifnot(is.character(x)))
        self$`theoretical_neutral_mass` <- `theoretical_neutral_mass`
      }
      if (!is.null(`adduct_ions`)) {
        stopifnot(is.vector(`adduct_ions`), length(`adduct_ions`) != 0)
        sapply(`adduct_ions`, function(x) stopifnot(is.character(x)))
        self$`adduct_ions` <- `adduct_ions`
      }
      if (!is.null(`reliability`)) {
        stopifnot(is.character(`reliability`), length(`reliability`) == 1)
        self$`reliability` <- `reliability`
      }
      if (!is.null(`best_id_confidence_measure`)) {
        stopifnot(R6::is.R6(`best_id_confidence_measure`))
        self$`best_id_confidence_measure` <- `best_id_confidence_measure`
      }
      if (!is.null(`best_id_confidence_value`)) {
        stopifnot(is.numeric(`best_id_confidence_value`), length(`best_id_confidence_value`) == 1)
        self$`best_id_confidence_value` <- `best_id_confidence_value`
      }
      if (!is.null(`abundance_assay`)) {
        stopifnot(is.vector(`abundance_assay`), length(`abundance_assay`) != 0)
        sapply(`abundance_assay`, function(x) stopifnot(is.character(x)))
        self$`abundance_assay` <- `abundance_assay`
      }
      if (!is.null(`abundance_study_variable`)) {
        stopifnot(is.vector(`abundance_study_variable`), length(`abundance_study_variable`) != 0)
        sapply(`abundance_study_variable`, function(x) stopifnot(is.character(x)))
        self$`abundance_study_variable` <- `abundance_study_variable`
      }
      if (!is.null(`abundance_variation_study_variable`)) {
        stopifnot(is.vector(`abundance_variation_study_variable`), length(`abundance_variation_study_variable`) != 0)
        sapply(`abundance_variation_study_variable`, function(x) stopifnot(is.character(x)))
        self$`abundance_variation_study_variable` <- `abundance_variation_study_variable`
      }
      if (!is.null(`opt`)) {
        stopifnot(is.vector(`opt`), length(`opt`) != 0)
        sapply(`opt`, function(x) stopifnot(R6::is.R6(x)))
        self$`opt` <- `opt`
      }
      if (!is.null(`comment`)) {
        stopifnot(is.vector(`comment`), length(`comment`) != 0)
        sapply(`comment`, function(x) stopifnot(R6::is.R6(x)))
        self$`comment` <- `comment`
      }
    },
    #' @description Serialize to list object suitable for jsonlite
    toJSON = function() {
      SmallMoleculeSummaryObject <- list()
      if (!is.null(self$`prefix`)) {
        SmallMoleculeSummaryObject[['prefix']] <-
          rmzTabM::safe_unbox(self$`prefix`)
      }
      if (!is.null(self$`header_prefix`)) {
        SmallMoleculeSummaryObject[['header_prefix']] <-
          rmzTabM::safe_unbox(self$`header_prefix`)
      }
      if (!is.null(self$`sml_id`)) {
        SmallMoleculeSummaryObject[['sml_id']] <-
          rmzTabM::safe_unbox(self$`sml_id`)
      }
      if (!is.null(self$`smf_id_refs`)) {
        SmallMoleculeSummaryObject[['smf_id_refs']] <-
          unlist(self$`smf_id_refs`)
      }
      if (!is.null(self$`database_identifier`)) {
        SmallMoleculeSummaryObject[['database_identifier']] <-
          unlist(self$`database_identifier`)
      }
      if (!is.null(self$`chemical_formula`)) {
        SmallMoleculeSummaryObject[['chemical_formula']] <-
          unlist(self$`chemical_formula`)
      }
      if (!is.null(self$`smiles`)) {
        SmallMoleculeSummaryObject[['smiles']] <-
          unlist(self$`smiles`)
      }
      if (!is.null(self$`inchi`)) {
        SmallMoleculeSummaryObject[['inchi']] <-
          unlist(self$`inchi`)
      }
      if (!is.null(self$`chemical_name`)) {
        SmallMoleculeSummaryObject[['chemical_name']] <-
          unlist(self$`chemical_name`)
      }
      if (!is.null(self$`uri`)) {
        SmallMoleculeSummaryObject[['uri']] <-
          unlist(self$`uri`)
      }
      if (!is.null(self$`theoretical_neutral_mass`)) {
        SmallMoleculeSummaryObject[['theoretical_neutral_mass']] <-
          unlist(self$`theoretical_neutral_mass`)
      }
      if (!is.null(self$`adduct_ions`)) {
        SmallMoleculeSummaryObject[['adduct_ions']] <-
          unlist(self$`adduct_ions`)
      }
      if (!is.null(self$`reliability`)) {
        SmallMoleculeSummaryObject[['reliability']] <-
          rmzTabM::safe_unbox(self$`reliability`)
      }
      if (!is.null(self$`best_id_confidence_measure`)) {
        SmallMoleculeSummaryObject[['best_id_confidence_measure']] <-
          self$`best_id_confidence_measure`$toJSON()
      }
      if (!is.null(self$`best_id_confidence_value`)) {
        SmallMoleculeSummaryObject[['best_id_confidence_value']] <-
          rmzTabM::safe_unbox(self$`best_id_confidence_value`)
      }
      if (!is.null(self$`abundance_assay`)) {
        SmallMoleculeSummaryObject[['abundance_assay']] <-
          unlist(self$`abundance_assay`)
      }
      if (!is.null(self$`abundance_study_variable`)) {
        SmallMoleculeSummaryObject[['abundance_study_variable']] <-
          unlist(self$`abundance_study_variable`)
      }
      if (!is.null(self$`abundance_variation_study_variable`)) {
        SmallMoleculeSummaryObject[['abundance_variation_study_variable']] <-
          unlist(self$`abundance_variation_study_variable`)
      }
      if (!is.null(self$`opt`)) {
        SmallMoleculeSummaryObject[['opt']] <-
          lapply(self$`opt`, function(x) x$toJSON())
      }
      if (!is.null(self$`comment`)) {
        SmallMoleculeSummaryObject[['comment']] <-
          lapply(self$`comment`, function(x) x$toJSON())
      }

      SmallMoleculeSummaryObject
    },
    #' @description Deserialize from jsonlite list object
    #' @param SmallMoleculeSummaryJson list object.
    fromJSON = function(SmallMoleculeSummaryJson) {
      SmallMoleculeSummaryObject <- jsonlite::fromJSON(SmallMoleculeSummaryJson)
      if (!is.null(SmallMoleculeSummaryObject$`prefix`)) {
        self$`prefix` <- SmallMoleculeSummaryObject$`prefix`
      }
      if (!is.null(SmallMoleculeSummaryObject$`header_prefix`)) {
        self$`header_prefix` <- SmallMoleculeSummaryObject$`header_prefix`
      }
      if (!is.null(SmallMoleculeSummaryObject$`sml_id`)) {
        self$`sml_id` <- SmallMoleculeSummaryObject$`sml_id`
      }
      if (!is.null(SmallMoleculeSummaryObject$`smf_id_refs`)) {
        self$`smf_id_refs` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`smf_id_refs`, "array[integer]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`database_identifier`)) {
        self$`database_identifier` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`database_identifier`, "array[character]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`chemical_formula`)) {
        self$`chemical_formula` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`chemical_formula`, "array[character]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`smiles`)) {
        self$`smiles` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`smiles`, "array[character]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`inchi`)) {
        self$`inchi` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`inchi`, "array[character]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`chemical_name`)) {
        self$`chemical_name` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`chemical_name`, "array[character]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`uri`)) {
        self$`uri` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`uri`, "array[character]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`theoretical_neutral_mass`)) {
        self$`theoretical_neutral_mass` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`theoretical_neutral_mass`, "array[numeric]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`adduct_ions`)) {
        self$`adduct_ions` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`adduct_ions`, "array[character]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`reliability`)) {
        self$`reliability` <- SmallMoleculeSummaryObject$`reliability`
      }
      if (!is.null(SmallMoleculeSummaryObject$`best_id_confidence_measure`)) {
        best_id_confidence_measureObject <- Parameter$new()
        best_id_confidence_measureObject$fromJSON(jsonlite::toJSON(SmallMoleculeSummaryObject$best_id_confidence_measure, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
        self$`best_id_confidence_measure` <- best_id_confidence_measureObject
      }
      if (!is.null(SmallMoleculeSummaryObject$`best_id_confidence_value`)) {
        self$`best_id_confidence_value` <- SmallMoleculeSummaryObject$`best_id_confidence_value`
      }
      if (!is.null(SmallMoleculeSummaryObject$`abundance_assay`)) {
        self$`abundance_assay` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`abundance_assay`, "array[numeric]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`abundance_study_variable`)) {
        self$`abundance_study_variable` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`abundance_study_variable`, "array[numeric]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`abundance_variation_study_variable`)) {
        self$`abundance_variation_study_variable` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`abundance_variation_study_variable`, "array[numeric]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`opt`)) {
        self$`opt` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`opt`, "array[OptColumnMapping]", loadNamespace("rmzTabM"))
      }
      if (!is.null(SmallMoleculeSummaryObject$`comment`)) {
        self$`comment` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`comment`, "array[Comment]", loadNamespace("rmzTabM"))
      }
    },
    #' @description Serialize to JSON string. 
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`prefix`)) {
        sprintf(
        '"prefix":
          "%s"
                ',
        rmzTabM::safe_unbox(self$`prefix`)
        )},
        if (!is.null(self$`header_prefix`)) {
        sprintf(
        '"header_prefix":
          "%s"
                ',
        rmzTabM::safe_unbox(self$`header_prefix`)
        )},
        if (!is.null(self$`sml_id`)) {
        sprintf(
        '"sml_id":
          %d
                ',
        rmzTabM::safe_unbox(self$`sml_id`)
        )},
        if (!is.null(self$`smf_id_refs`)) {
        sprintf(
        '"smf_id_refs":
           [%s]
        ',
        paste(unlist(lapply(self$`smf_id_refs`, function(x) paste0(x))), collapse=",")
        )},
        if (!is.null(self$`database_identifier`)) {
        sprintf(
        '"database_identifier":
           [%s]
        ',
        paste(unlist(lapply(self$`database_identifier`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`chemical_formula`)) {
        sprintf(
        '"chemical_formula":
           [%s]
        ',
        paste(unlist(lapply(self$`chemical_formula`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`smiles`)) {
        sprintf(
        '"smiles":
           [%s]
        ',
        paste(unlist(lapply(self$`smiles`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`inchi`)) {
        sprintf(
        '"inchi":
           [%s]
        ',
        paste(unlist(lapply(self$`inchi`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`chemical_name`)) {
        sprintf(
        '"chemical_name":
           [%s]
        ',
        paste(unlist(lapply(self$`chemical_name`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`uri`)) {
        sprintf(
        '"uri":
           [%s]
        ',
        paste(unlist(lapply(self$`uri`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`theoretical_neutral_mass`)) {
        sprintf(
        '"theoretical_neutral_mass":
           [%s]
        ',
        paste(unlist(lapply(self$`theoretical_neutral_mass`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`adduct_ions`)) {
        sprintf(
        '"adduct_ions":
           [%s]
        ',
        paste(unlist(lapply(self$`adduct_ions`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`reliability`)) {
        sprintf(
        '"reliability":
          "%s"
                ',
        rmzTabM::safe_unbox(self$`reliability`)
        )},
        if (!is.null(self$`best_id_confidence_measure`)) {
        sprintf(
        '"best_id_confidence_measure":
        %s
        ',
        jsonlite::toJSON(self$`best_id_confidence_measure`$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)
        )},
        if (!is.null(self$`best_id_confidence_value`)) {
        sprintf(
        '"best_id_confidence_value":
          %s
                ',
        rmzTabM::safe_unbox(self$`best_id_confidence_value`)
        )},
        if (!is.null(self$`abundance_assay`)) {
        sprintf(
        '"abundance_assay":
           [%s]
        ',
        paste(unlist(lapply(self$`abundance_assay`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`abundance_study_variable`)) {
        sprintf(
        '"abundance_study_variable":
           [%s]
        ',
        paste(unlist(lapply(self$`abundance_study_variable`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`abundance_variation_study_variable`)) {
        sprintf(
        '"abundance_variation_study_variable":
           [%s]
        ',
        paste(unlist(lapply(self$`abundance_variation_study_variable`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`opt`)) {
        sprintf(
        '"opt":
        [%s]
',
        paste(sapply(self$`opt`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)), collapse=",")
        )},
        if (!is.null(self$`comment`)) {
        sprintf(
        '"comment":
        [%s]
',
        paste(sapply(self$`comment`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)), collapse=",")
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    #' @description Deserialize from JSON string
    #' @param SmallMoleculeSummaryJson SmallMoleculeSummaryJson string
    fromJSONString = function(SmallMoleculeSummaryJson) {
      SmallMoleculeSummaryObject <- jsonlite::fromJSON(SmallMoleculeSummaryJson)
      self$`prefix` <- SmallMoleculeSummaryObject$`prefix`
      self$`header_prefix` <- SmallMoleculeSummaryObject$`header_prefix`
      self$`sml_id` <- SmallMoleculeSummaryObject$`sml_id`
      self$`smf_id_refs` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`smf_id_refs`, "array[integer]", loadNamespace("rmzTabM"))
      self$`database_identifier` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`database_identifier`, "array[character]", loadNamespace("rmzTabM"))
      self$`chemical_formula` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`chemical_formula`, "array[character]", loadNamespace("rmzTabM"))
      self$`smiles` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`smiles`, "array[character]", loadNamespace("rmzTabM"))
      self$`inchi` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`inchi`, "array[character]", loadNamespace("rmzTabM"))
      self$`chemical_name` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`chemical_name`, "array[character]", loadNamespace("rmzTabM"))
      self$`uri` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`uri`, "array[character]", loadNamespace("rmzTabM"))
      self$`theoretical_neutral_mass` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`theoretical_neutral_mass`, "array[numeric]", loadNamespace("rmzTabM"))
      self$`adduct_ions` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`adduct_ions`, "array[character]", loadNamespace("rmzTabM"))
      self$`reliability` <- SmallMoleculeSummaryObject$`reliability`
      self$`best_id_confidence_measure` <- Parameter$new()$fromJSONString(jsonlite::toJSON(SmallMoleculeSummaryObject$best_id_confidence_measure, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
      self$`best_id_confidence_value` <- SmallMoleculeSummaryObject$`best_id_confidence_value`
      self$`abundance_assay` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`abundance_assay`, "array[numeric]", loadNamespace("rmzTabM"))
      self$`abundance_study_variable` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`abundance_study_variable`, "array[numeric]", loadNamespace("rmzTabM"))
      self$`abundance_variation_study_variable` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`abundance_variation_study_variable`, "array[numeric]", loadNamespace("rmzTabM"))
      self$`opt` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`opt`, "array[OptColumnMapping]", loadNamespace("rmzTabM"))
      self$`comment` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`comment`, "array[Comment]", loadNamespace("rmzTabM"))
      self
    },
    #' @description Serialize to data frame
    toDataFrame = function() {
      fixed_header_values <- c(
        "SMH"=self$`prefix`,
        "SML_ID"=self$`sml_id`,	
        "SMF_ID_REFS"=valueOrDefault(unlist(self$`smf_id_refs`), FUN=paste, collapse="|"),
        "chemical_name"=valueOrDefault(unlist(self$`chemical_name`), FUN=paste, collapse="|"),
        "database_identifier"=valueOrDefault(unlist(self$`database_identifier`), FUN=paste, collapse="|"),
        "chemical_formula"=valueOrDefault(unlist(self$`chemical_formula`), FUN=paste, collapse="|"),
        "smiles"=valueOrDefault(unlist(self$`smiles`), FUN=paste, collapse="|"),
        "inchi"=valueOrDefault(unlist(self$`inchi`), FUN=paste, collapse="|"),
        "uri"=valueOrDefault(unlist(self$`uri`), FUN=paste, collapse="|"),
        "theoretical_neutral_mass"=valueOrDefault(unlist(self$`theoretical_neutral_mass`), FUN=paste, collapse="|"),
        "adduct_ions"=valueOrDefault(unlist(self$`adduct_ions`), FUN=paste, collapse="|"),
        "reliability"=valueOrDefault(self$`reliability`),
        "best_id_confidence_measure"=valueOrDefault(self$`best_id_confidence_measure`, FUN=function(x){x$toString()}),
        "best_id_confidence_value"=valueOrDefault(self$`best_id_confidence_value`)
      )
      abundance_assay <-
        unlist(lapply(seq_along(self$`abundance_assay`), function(idx, x) {
          paste0("abundance_assay[", idx, "]")
        }, x = self$`abundance_assay`))
      abundance_assay_values <-
        unlist(lapply(seq_along(self$`abundance_assay`), function(idx, x) {
          valueOrDefault(x)
        }, x = self$`abundance_assay`))
      names(abundance_assay_values) <- abundance_assay
      
      abundance_study_variable <-
        unlist(lapply(seq_along(self$`abundance_study_variable`), function(idx, x) {
          paste0("abundance_study_variable[", idx, "]")
        }, x = self$`abundance_study_variable`))
      abundance_study_variable_values <-
        unlist(lapply(seq_along(self$`abundance_study_variable`), function(idx, x) {
          valueOrDefault(x)
        }, x = self$`abundance_study_variable`))
      names(abundance_study_variable_values) <- abundance_study_variable
      
      abundance_variation_study_variable <-
        unlist(lapply(seq_along(self$`abundance_variation_study_variable`), function(idx, x) {
          paste0("abundance_variation_study_variable[", idx, "]")
        }, x = self$`abundance_variation_study_variable`))
      abundance_variation_study_variable_values <-
        unlist(lapply(seq_along(self$`abundance_variation_study_variable`), function(idx, x) {
          valueOrDefault(x)
        }, x = self$`abundance_variation_study_variable`))
      names(abundance_variation_study_variable_values) <- abundance_variation_study_variable
      
      opt <-
        unlist(lapply(self$`opt`, function(x) {
          x$toString()
        }))
      opt_values <- 
        unlist(lapply(self$`opt`, function(x) {
          valueOrDefault(x$`value`)
        }))
      names(opt_values) <- opt
      entries <-
        as.data.frame(
          t(
            c(
              fixed_header_values,
              abundance_assay_values,
              abundance_study_variable_values,
              abundance_variation_study_variable_values,
              opt_values
            )
          )
        )
      entries
    },
    #' @description Deserialize from summary data frame
    #' @param SummaryDataFrame Summary data frame
    fromDataFrame = function(SummaryDataFrame) {
      stopifnot(nrow(SummaryDataFrame)==1)
      columnNames <- colnames(SummaryDataFrame)
      self$`prefix` <- "SML"
      self$`header_prefix` <- "SMH"
      if (rlang::has_name(SummaryDataFrame, "sml_id")) {
        self$`sml_id` <- as.numeric(SummaryDataFrame$`sml_id`)
      }
      if (rlang::has_name(SummaryDataFrame, "SML_ID")) {
        self$`sml_id` <- as.numeric(SummaryDataFrame$`SML_ID`)
      }
      if (rlang::has_name(SummaryDataFrame, "smf_id_refs")) {
        refList <- splitList(SummaryDataFrame$`smf_id_refs`)
        self$`smf_id_refs` <- lapply(refList, function(x) {
          extractId(x)  
        })
      }
      if (rlang::has_name(SummaryDataFrame, "SMF_ID_REFS")) {
        refList <- splitList(SummaryDataFrame$`SMF_ID_REFS`)
        self$`smf_id_refs` <- lapply(refList, function(x) {
          extractId(x)  
        })
      }
      if (rlang::has_name(SummaryDataFrame, "database_identifier")) {
        refList <- splitList(SummaryDataFrame$`database_identifier`)
        self$`database_identifier` <- lapply(refList, function(x) {x})
      }
      if (rlang::has_name(SummaryDataFrame, "chemical_formula")) {
        refList <- splitList(SummaryDataFrame$`chemical_formula`)
        self$`chemical_formula` <- lapply(refList, function(x) {x})
      }
      if (rlang::has_name(SummaryDataFrame, "smiles")) {
        refList <- splitList(SummaryDataFrame$`smiles`)
        self$`smiles` <- lapply(refList, function(x) {x})
      }
      if (rlang::has_name(SummaryDataFrame, "inchi")) {
        refList <- splitList(SummaryDataFrame$`inchi`)
        self$`inchi` <- lapply(refList, function(x) {x})
      }
      if (rlang::has_name(SummaryDataFrame, "chemical_name")) {
        refList <- splitList(SummaryDataFrame$`chemical_name`)
        self$`chemical_name` <- lapply(refList, function(x) {x})
      }
      if (rlang::has_name(SummaryDataFrame, "uri")) {
        refList <- splitList(SummaryDataFrame$`uri`)
        self$`uri` <- lapply(refList, function(x) {x})
      }
      if (rlang::has_name(SummaryDataFrame, "theoretical_neutral_mass")) {
        refList <- splitList(SummaryDataFrame$`theoretical_neutral_mass`)
        self$`theoretical_neutral_mass` <- lapply(refList, function(x) {
          as.numeric(x)  
        })
      }
      if (rlang::has_name(SummaryDataFrame, "adduct_ions")) {
        refList <- splitList(SummaryDataFrame$`adduct_ions`)
        self$`adduct_ions` <- lapply(refList, function(x) {x})
      }
      if (rlang::has_name(SummaryDataFrame, "reliability")) {
        self$`reliability` <- SummaryDataFrame$`reliability`
      }
      if (rlang::has_name(SummaryDataFrame, "best_id_confidence_measure")) {
        param <- Parameter$new()
        self$`best_id_confidence_measure` <- param$fromString(NULL, SummaryDataFrame$`best_id_confidence_measure`)
      }
      if (rlang::has_name(SummaryDataFrame, "best_id_confidence_value")) {
        self$`best_id_confidence_value` <- SummaryDataFrame$`best_id_confidence_value`
      }
      abundance_assay_df <- SummaryDataFrame %>% dplyr::select(dplyr::starts_with("abundance_assay"))
      if (!is.null(dim(abundance_assay_df)) && dim(abundance_assay_df)[1] > 0) {
        self$`abundance_assay` <- unlist(lapply(abundance_assay_df, function(x) {
          as.numeric(x)
        }))
      }
      abundance_assay_sv <- SummaryDataFrame %>% dplyr::select(dplyr::starts_with("abundance_study_variable"))
      if (!is.null(dim(abundance_assay_sv)) && dim(abundance_assay_sv)[1] > 0) {
        self$`abundance_study_variable` <- unlist(lapply(abundance_assay_sv, function(x) {
          as.numeric(x)
        }))
      }
      abundance_variation_sv <- SummaryDataFrame %>% dplyr::select(dplyr::starts_with("abundance_variation_study_variable"))
      if (!is.null(dim(abundance_variation_sv)) && dim(abundance_variation_sv)[1] > 0) {
        self$`abundance_variation_study_variable` <- unlist(lapply(abundance_variation_sv, function(x) {
          as.numeric(x)
        }))
      }
      
      # TODO: Missing handling of optional columns
      # self$`opt` <- ApiClient$new()$deserializeObj(SmallMoleculeSummaryObject$`opt`, "array[OptColumnMapping]", loadNamespace("rmzTabM"))
      opt_cols <- SummaryDataFrame %>% dplyr::select(dplyr::starts_with("opt_"))
      if (!is.null(dim(opt_cols)) && dim(opt_cols)[1] > 0) {
        warning("Handling of Optional columns not yet implemented")
      }
      
      if (rlang::has_name(SummaryDataFrame, "comment")) {
        comment <- Comment$new()
        self$`comment` <- comment$fromDataFrame(SummaryDataFrame$`comment`)
      }
      self
    }
  )
)
