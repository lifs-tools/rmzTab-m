# mzTab-M reference implementation and validation API.
# 
# This is the mzTab-M reference implementation and validation API service.
# 
# OpenAPI spec version: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://github.com/swagger-api/swagger-codegen.git


#' SmallMoleculeFeature Class
#'
#' The small molecule feature section is table-based, representing individual MS regions (generally considered to be the elution profile for all isotopomers formed from a single charge state of a molecule), that have been measured/quantified. However, for approaches that quantify individual isotopomers e.g. stable isotope labelling/flux studies, then each SMF row SHOULD represent a single isotopomer.  Different adducts or derivatives and different charge states of individual molecules should be reported as separate SMF rows.  The small molecule feature section MUST always come after the Small Molecule Table. All table columns MUST be Tab separated. There MUST NOT be any empty cells. Missing values MUST be reported using “null”.  The order of columns MUST follow the order specified below.  All columns are MANDATORY except for “opt_” columns. 
#'
#' @field prefix 
#' @field header_prefix 
#' @field smf_id 
#' @field sme_id_refs 
#' @field sme_id_ref_ambiguity_code 
#' @field adduct_ion 
#' @field isotopomer 
#' @field exp_mass_to_charge 
#' @field charge 
#' @field retention_time_in_seconds 
#' @field retention_time_in_seconds_start 
#' @field retention_time_in_seconds_end 
#' @field abundance_assay 
#' @field opt 
#' @field comment 
#'
#' 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SmallMoleculeFeature <- R6::R6Class(
  'SmallMoleculeFeature',
  public = list(
    `prefix` = NULL,
    `header_prefix` = NULL,
    `smf_id` = NULL,
    `sme_id_refs` = NULL,
    `sme_id_ref_ambiguity_code` = NULL,
    `adduct_ion` = NULL,
    `isotopomer` = NULL,
    `exp_mass_to_charge` = NULL,
    `charge` = NULL,
    `retention_time_in_seconds` = NULL,
    `retention_time_in_seconds_start` = NULL,
    `retention_time_in_seconds_end` = NULL,
    `abundance_assay` = NULL,
    `opt` = NULL,
    `comment` = NULL,
    initialize = function(`prefix`, `header_prefix`, `smf_id`, `sme_id_refs`, `sme_id_ref_ambiguity_code`, `adduct_ion`, `isotopomer`, `exp_mass_to_charge`, `charge`, `retention_time_in_seconds`, `retention_time_in_seconds_start`, `retention_time_in_seconds_end`, `abundance_assay`, `opt`, `comment`){
      if (!missing(`prefix`)) {
        stopifnot(is.character(`prefix`), length(`prefix`) == 1)
        self$`prefix` <- `prefix`
      }
      if (!missing(`header_prefix`)) {
        stopifnot(is.character(`header_prefix`), length(`header_prefix`) == 1)
        self$`header_prefix` <- `header_prefix`
      }
      if (!missing(`smf_id`)) {
        stopifnot(is.numeric(`smf_id`), length(`smf_id`) == 1)
        self$`smf_id` <- `smf_id`
      }
      if (!missing(`sme_id_refs`)) {
        stopifnot(is.list(`sme_id_refs`), length(`sme_id_refs`) != 0)
        lapply(`sme_id_refs`, function(x) stopifnot(is.character(x)))
        self$`sme_id_refs` <- `sme_id_refs`
      }
      if (!missing(`sme_id_ref_ambiguity_code`)) {
        stopifnot(is.numeric(`sme_id_ref_ambiguity_code`), length(`sme_id_ref_ambiguity_code`) == 1)
        self$`sme_id_ref_ambiguity_code` <- `sme_id_ref_ambiguity_code`
      }
      if (!missing(`adduct_ion`)) {
        stopifnot(is.character(`adduct_ion`), length(`adduct_ion`) == 1)
        self$`adduct_ion` <- `adduct_ion`
      }
      if (!missing(`isotopomer`)) {
        stopifnot(R6::is.R6(`isotopomer`))
        self$`isotopomer` <- `isotopomer`
      }
      if (!missing(`exp_mass_to_charge`)) {
        stopifnot(is.numeric(`exp_mass_to_charge`), length(`exp_mass_to_charge`) == 1)
        self$`exp_mass_to_charge` <- `exp_mass_to_charge`
      }
      if (!missing(`charge`)) {
        stopifnot(is.numeric(`charge`), length(`charge`) == 1)
        self$`charge` <- `charge`
      }
      if (!missing(`retention_time_in_seconds`)) {
        stopifnot(is.numeric(`retention_time_in_seconds`), length(`retention_time_in_seconds`) == 1)
        self$`retention_time_in_seconds` <- `retention_time_in_seconds`
      }
      if (!missing(`retention_time_in_seconds_start`)) {
        stopifnot(is.numeric(`retention_time_in_seconds_start`), length(`retention_time_in_seconds_start`) == 1)
        self$`retention_time_in_seconds_start` <- `retention_time_in_seconds_start`
      }
      if (!missing(`retention_time_in_seconds_end`)) {
        stopifnot(is.numeric(`retention_time_in_seconds_end`), length(`retention_time_in_seconds_end`) == 1)
        self$`retention_time_in_seconds_end` <- `retention_time_in_seconds_end`
      }
      if (!missing(`abundance_assay`)) {
        stopifnot(is.list(`abundance_assay`), length(`abundance_assay`) != 0)
        lapply(`abundance_assay`, function(x) stopifnot(is.character(x)))
        self$`abundance_assay` <- `abundance_assay`
      }
      if (!missing(`opt`)) {
        stopifnot(is.list(`opt`), length(`opt`) != 0)
        lapply(`opt`, function(x) stopifnot(R6::is.R6(x)))
        self$`opt` <- `opt`
      }
      if (!missing(`comment`)) {
        stopifnot(is.list(`comment`), length(`comment`) != 0)
        lapply(`comment`, function(x) stopifnot(R6::is.R6(x)))
        self$`comment` <- `comment`
      }
    },
    toJSON = function() {
      SmallMoleculeFeatureObject <- list()
      if (!is.null(self$`prefix`)) {
        SmallMoleculeFeatureObject[['prefix']] <- self$`prefix`
      }
      if (!is.null(self$`header_prefix`)) {
        SmallMoleculeFeatureObject[['header_prefix']] <- self$`header_prefix`
      }
      if (!is.null(self$`smf_id`)) {
        SmallMoleculeFeatureObject[['smf_id']] <- self$`smf_id`
      }
      if (!is.null(self$`sme_id_refs`)) {
        SmallMoleculeFeatureObject[['sme_id_refs']] <- self$`sme_id_refs`
      }
      if (!is.null(self$`sme_id_ref_ambiguity_code`)) {
        SmallMoleculeFeatureObject[['sme_id_ref_ambiguity_code']] <- self$`sme_id_ref_ambiguity_code`
      }
      if (!is.null(self$`adduct_ion`)) {
        SmallMoleculeFeatureObject[['adduct_ion']] <- self$`adduct_ion`
      }
      if (!is.null(self$`isotopomer`)) {
        SmallMoleculeFeatureObject[['isotopomer']] <- self$`isotopomer`$toJSON()
      }
      if (!is.null(self$`exp_mass_to_charge`)) {
        SmallMoleculeFeatureObject[['exp_mass_to_charge']] <- self$`exp_mass_to_charge`
      }
      if (!is.null(self$`charge`)) {
        SmallMoleculeFeatureObject[['charge']] <- self$`charge`
      }
      if (!is.null(self$`retention_time_in_seconds`)) {
        SmallMoleculeFeatureObject[['retention_time_in_seconds']] <- self$`retention_time_in_seconds`
      }
      if (!is.null(self$`retention_time_in_seconds_start`)) {
        SmallMoleculeFeatureObject[['retention_time_in_seconds_start']] <- self$`retention_time_in_seconds_start`
      }
      if (!is.null(self$`retention_time_in_seconds_end`)) {
        SmallMoleculeFeatureObject[['retention_time_in_seconds_end']] <- self$`retention_time_in_seconds_end`
      }
      if (!is.null(self$`abundance_assay`)) {
        SmallMoleculeFeatureObject[['abundance_assay']] <- self$`abundance_assay`
      }
      if (!is.null(self$`opt`)) {
        SmallMoleculeFeatureObject[['opt']] <- lapply(self$`opt`, function(x) x$toJSON())
      }
      if (!is.null(self$`comment`)) {
        SmallMoleculeFeatureObject[['comment']] <- lapply(self$`comment`, function(x) x$toJSON())
      }

      SmallMoleculeFeatureObject
    },
    fromJSON = function(SmallMoleculeFeatureJson) {
      SmallMoleculeFeatureObject <- jsonlite::fromJSON(SmallMoleculeFeatureJson, simplifyVector = FALSE)
      if (!is.null(SmallMoleculeFeatureObject$`prefix`)) {
        self$`prefix` <- SmallMoleculeFeatureObject$`prefix`
      }
      if (!is.null(SmallMoleculeFeatureObject$`header_prefix`)) {
        self$`header_prefix` <- SmallMoleculeFeatureObject$`header_prefix`
      }
      if (!is.null(SmallMoleculeFeatureObject$`smf_id`)) {
        self$`smf_id` <- SmallMoleculeFeatureObject$`smf_id`
      }
      if (!is.null(SmallMoleculeFeatureObject$`sme_id_refs`)) {
        self$`sme_id_refs` <- SmallMoleculeFeatureObject$`sme_id_refs`
      }
      if (!is.null(SmallMoleculeFeatureObject$`sme_id_ref_ambiguity_code`)) {
        self$`sme_id_ref_ambiguity_code` <- SmallMoleculeFeatureObject$`sme_id_ref_ambiguity_code`
      }
      if (!is.null(SmallMoleculeFeatureObject$`adduct_ion`)) {
        self$`adduct_ion` <- SmallMoleculeFeatureObject$`adduct_ion`
      }
      if (!is.null(SmallMoleculeFeatureObject$`isotopomer`)) {
        `isotopomerObject` <- Parameter$new()
        `isotopomerObject`$fromJSON(jsonlite::toJSON(SmallMoleculeFeatureObject$isotopomer, auto_unbox = TRUE))
        self$`isotopomer` <- `isotopomerObject`
      }
      if (!is.null(SmallMoleculeFeatureObject$`exp_mass_to_charge`)) {
        self$`exp_mass_to_charge` <- SmallMoleculeFeatureObject$`exp_mass_to_charge`
      }
      if (!is.null(SmallMoleculeFeatureObject$`charge`)) {
        self$`charge` <- SmallMoleculeFeatureObject$`charge`
      }
      if (!is.null(SmallMoleculeFeatureObject$`retention_time_in_seconds`)) {
        self$`retention_time_in_seconds` <- SmallMoleculeFeatureObject$`retention_time_in_seconds`
      }
      if (!is.null(SmallMoleculeFeatureObject$`retention_time_in_seconds_start`)) {
        self$`retention_time_in_seconds_start` <- SmallMoleculeFeatureObject$`retention_time_in_seconds_start`
      }
      if (!is.null(SmallMoleculeFeatureObject$`retention_time_in_seconds_end`)) {
        self$`retention_time_in_seconds_end` <- SmallMoleculeFeatureObject$`retention_time_in_seconds_end`
      }
      if (!is.null(SmallMoleculeFeatureObject$`abundance_assay`)) {
        self$`abundance_assay` <- SmallMoleculeFeatureObject$`abundance_assay`
      }
      if (!is.null(SmallMoleculeFeatureObject$`opt`)) {
        self$`opt` <- lapply(SmallMoleculeFeatureObject$`opt`, function(x) {
          `optObject` <- OptColumnMapping$new()
          `optObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `optObject`
        })
      }
      if (!is.null(SmallMoleculeFeatureObject$`comment`)) {
        self$`comment` <- lapply(SmallMoleculeFeatureObject$`comment`, function(x) {
          `commentObject` <- Comment$new()
          `commentObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `commentObject`
        })
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "prefix": %s,
           "header_prefix": %s,
           "smf_id": %s,
           "sme_id_refs": [%s],
           "sme_id_ref_ambiguity_code": %s,
           "adduct_ion": %s,
           "isotopomer": %s,
           "exp_mass_to_charge": %d,
           "charge": %s,
           "retention_time_in_seconds": %d,
           "retention_time_in_seconds_start": %d,
           "retention_time_in_seconds_end": %d,
           "abundance_assay": [%s],
           "opt": [%s],
           "comment": [%s]
        }',
        self$`prefix`,
        self$`header_prefix`,
        self$`smf_id`,
        lapply(self$`sme_id_refs`, function(x) paste(paste0('"', x, '"'), sep=",")),
        self$`sme_id_ref_ambiguity_code`,
        self$`adduct_ion`,
        self$`isotopomer`$toJSON(),
        self$`exp_mass_to_charge`,
        self$`charge`,
        self$`retention_time_in_seconds`,
        self$`retention_time_in_seconds_start`,
        self$`retention_time_in_seconds_end`,
        lapply(self$`abundance_assay`, function(x) paste(paste0('"', x, '"'), sep=",")),
        lapply(self$`opt`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`comment`, function(x) paste(x$toJSON(), sep=","))
      )
    },
    fromJSONString = function(SmallMoleculeFeatureJson) {
      SmallMoleculeFeatureObject <- jsonlite::fromJSON(SmallMoleculeFeatureJson, simplifyVector = FALSE)
      self$`prefix` <- SmallMoleculeFeatureObject$`prefix`
      self$`header_prefix` <- SmallMoleculeFeatureObject$`header_prefix`
      self$`smf_id` <- SmallMoleculeFeatureObject$`smf_id`
      self$`sme_id_refs` <- SmallMoleculeFeatureObject$`sme_id_refs`
      self$`sme_id_ref_ambiguity_code` <- SmallMoleculeFeatureObject$`sme_id_ref_ambiguity_code`
      self$`adduct_ion` <- SmallMoleculeFeatureObject$`adduct_ion`
      ParameterObject <- Parameter$new()
      self$`isotopomer` <- ParameterObject$fromJSON(jsonlite::toJSON(SmallMoleculeFeatureObject$isotopomer, auto_unbox = TRUE))
      self$`exp_mass_to_charge` <- SmallMoleculeFeatureObject$`exp_mass_to_charge`
      self$`charge` <- SmallMoleculeFeatureObject$`charge`
      self$`retention_time_in_seconds` <- SmallMoleculeFeatureObject$`retention_time_in_seconds`
      self$`retention_time_in_seconds_start` <- SmallMoleculeFeatureObject$`retention_time_in_seconds_start`
      self$`retention_time_in_seconds_end` <- SmallMoleculeFeatureObject$`retention_time_in_seconds_end`
      self$`abundance_assay` <- SmallMoleculeFeatureObject$`abundance_assay`
      self$`opt` <- lapply(SmallMoleculeFeatureObject$`opt`, function(x) OptColumnMapping$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`comment` <- lapply(SmallMoleculeFeatureObject$`comment`, function(x) Comment$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
    }
  )
)
