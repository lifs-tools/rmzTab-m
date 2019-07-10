# mzTab-M reference implementation and validation API.
# 
# This is the mzTab-M reference implementation and validation API service.
# 
# OpenAPI spec version: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://github.com/swagger-api/swagger-codegen.git


#' Metadata Class
#'
#' The metadata section provides additional information about the dataset(s) reported in the mzTab file. All fields in the metadata section are optional apart from those noted as mandatory. The fields in the metadata section MUST be reported in order of the various fields listed here. The field’s name and value MUST be separated by a tab character.  
#'
#' @field prefix 
#' @field mzTab-version 
#' @field mzTab-ID 
#' @field title 
#' @field description 
#' @field sample_processing 
#' @field instrument 
#' @field software 
#' @field publication 
#' @field contact 
#' @field uri 
#' @field external_study_uri 
#' @field quantification_method 
#' @field study_variable 
#' @field ms_run 
#' @field assay 
#' @field sample 
#' @field custom 
#' @field cv 
#' @field database 
#' @field derivatization_agent 
#' @field small_molecule-quantification_unit 
#' @field small_molecule_feature-quantification_unit 
#' @field small_molecule-identification_reliability 
#' @field id_confidence_measure 
#' @field colunit-small_molecule 
#' @field colunit-small_molecule_feature 
#' @field colunit-small_molecule_evidence 
#'
#' mzTab-M specification example(s)
#' \preformatted{
#' MTD	mzTab-version	2.0.0-M
#' MTD	mzTab-ID	MTBL1234
#' MTD	title	Effects of Rapamycin on metabolite profile
#' ...
#' 
#' }
#' 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Metadata <- R6::R6Class(
  'Metadata',
  public = list(
    `prefix` = NULL,
    `mzTab-version` = NULL,
    `mzTab-ID` = NULL,
    `title` = NULL,
    `description` = NULL,
    `sample_processing` = NULL,
    `instrument` = NULL,
    `software` = NULL,
    `publication` = NULL,
    `contact` = NULL,
    `uri` = NULL,
    `external_study_uri` = NULL,
    `quantification_method` = NULL,
    `study_variable` = NULL,
    `ms_run` = NULL,
    `assay` = NULL,
    `sample` = NULL,
    `custom` = NULL,
    `cv` = NULL,
    `database` = NULL,
    `derivatization_agent` = NULL,
    `small_molecule-quantification_unit` = NULL,
    `small_molecule_feature-quantification_unit` = NULL,
    `small_molecule-identification_reliability` = NULL,
    `id_confidence_measure` = NULL,
    `colunit-small_molecule` = NULL,
    `colunit-small_molecule_feature` = NULL,
    `colunit-small_molecule_evidence` = NULL,
    initialize = function(`prefix`, `mzTab-version`, `mzTab-ID`, `title`, `description`, `sample_processing`, `instrument`, `software`, `publication`, `contact`, `uri`, `external_study_uri`, `quantification_method`, `study_variable`, `ms_run`, `assay`, `sample`, `custom`, `cv`, `database`, `derivatization_agent`, `small_molecule-quantification_unit`, `small_molecule_feature-quantification_unit`, `small_molecule-identification_reliability`, `id_confidence_measure`, `colunit-small_molecule`, `colunit-small_molecule_feature`, `colunit-small_molecule_evidence`){
      if (!missing(`prefix`)) {
        stopifnot(is.character(`prefix`), length(`prefix`) == 1)
        self$`prefix` <- `prefix`
      }
      if (!missing(`mzTab-version`)) {
        stopifnot(is.character(`mzTab-version`), length(`mzTab-version`) == 1)
        self$`mzTab-version` <- `mzTab-version`
      }
      if (!missing(`mzTab-ID`)) {
        stopifnot(is.character(`mzTab-ID`), length(`mzTab-ID`) == 1)
        self$`mzTab-ID` <- `mzTab-ID`
      }
      if (!missing(`title`)) {
        stopifnot(is.character(`title`), length(`title`) == 1)
        self$`title` <- `title`
      }
      if (!missing(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
        self$`description` <- `description`
      }
      if (!missing(`sample_processing`)) {
        stopifnot(is.list(`sample_processing`), length(`sample_processing`) != 0)
        lapply(`sample_processing`, function(x) stopifnot(R6::is.R6(x)))
        self$`sample_processing` <- `sample_processing`
      }
      if (!missing(`instrument`)) {
        stopifnot(is.list(`instrument`), length(`instrument`) != 0)
        lapply(`instrument`, function(x) stopifnot(R6::is.R6(x)))
        self$`instrument` <- `instrument`
      }
      if (!missing(`software`)) {
        stopifnot(is.list(`software`), length(`software`) != 0)
        lapply(`software`, function(x) stopifnot(R6::is.R6(x)))
        self$`software` <- `software`
      }
      if (!missing(`publication`)) {
        stopifnot(is.list(`publication`), length(`publication`) != 0)
        lapply(`publication`, function(x) stopifnot(R6::is.R6(x)))
        self$`publication` <- `publication`
      }
      if (!missing(`contact`)) {
        stopifnot(is.list(`contact`), length(`contact`) != 0)
        lapply(`contact`, function(x) stopifnot(R6::is.R6(x)))
        self$`contact` <- `contact`
      }
      if (!missing(`uri`)) {
        stopifnot(is.list(`uri`), length(`uri`) != 0)
        lapply(`uri`, function(x) stopifnot(R6::is.R6(x)))
        self$`uri` <- `uri`
      }
      if (!missing(`external_study_uri`)) {
        stopifnot(is.list(`external_study_uri`), length(`external_study_uri`) != 0)
        lapply(`external_study_uri`, function(x) stopifnot(R6::is.R6(x)))
        self$`external_study_uri` <- `external_study_uri`
      }
      if (!missing(`quantification_method`)) {
        stopifnot(R6::is.R6(`quantification_method`))
        self$`quantification_method` <- `quantification_method`
      }
      if (!missing(`study_variable`)) {
        stopifnot(is.list(`study_variable`), length(`study_variable`) != 0)
        lapply(`study_variable`, function(x) stopifnot(R6::is.R6(x)))
        self$`study_variable` <- `study_variable`
      }
      if (!missing(`ms_run`)) {
        stopifnot(is.list(`ms_run`), length(`ms_run`) != 0)
        lapply(`ms_run`, function(x) stopifnot(R6::is.R6(x)))
        self$`ms_run` <- `ms_run`
      }
      if (!missing(`assay`)) {
        stopifnot(is.list(`assay`), length(`assay`) != 0)
        lapply(`assay`, function(x) stopifnot(R6::is.R6(x)))
        self$`assay` <- `assay`
      }
      if (!missing(`sample`)) {
        stopifnot(is.list(`sample`), length(`sample`) != 0)
        lapply(`sample`, function(x) stopifnot(R6::is.R6(x)))
        self$`sample` <- `sample`
      }
      if (!missing(`custom`)) {
        stopifnot(is.list(`custom`), length(`custom`) != 0)
        lapply(`custom`, function(x) stopifnot(R6::is.R6(x)))
        self$`custom` <- `custom`
      }
      if (!missing(`cv`)) {
        stopifnot(is.list(`cv`), length(`cv`) != 0)
        lapply(`cv`, function(x) stopifnot(R6::is.R6(x)))
        self$`cv` <- `cv`
      }
      if (!missing(`database`)) {
        stopifnot(is.list(`database`), length(`database`) != 0)
        lapply(`database`, function(x) stopifnot(R6::is.R6(x)))
        self$`database` <- `database`
      }
      if (!missing(`derivatization_agent`)) {
        stopifnot(is.list(`derivatization_agent`), length(`derivatization_agent`) != 0)
        lapply(`derivatization_agent`, function(x) stopifnot(R6::is.R6(x)))
        self$`derivatization_agent` <- `derivatization_agent`
      }
      if (!missing(`small_molecule-quantification_unit`)) {
        stopifnot(R6::is.R6(`small_molecule-quantification_unit`))
        self$`small_molecule-quantification_unit` <- `small_molecule-quantification_unit`
      }
      if (!missing(`small_molecule_feature-quantification_unit`)) {
        stopifnot(R6::is.R6(`small_molecule_feature-quantification_unit`))
        self$`small_molecule_feature-quantification_unit` <- `small_molecule_feature-quantification_unit`
      }
      if (!missing(`small_molecule-identification_reliability`)) {
        stopifnot(R6::is.R6(`small_molecule-identification_reliability`))
        self$`small_molecule-identification_reliability` <- `small_molecule-identification_reliability`
      }
      if (!missing(`id_confidence_measure`)) {
        stopifnot(is.list(`id_confidence_measure`), length(`id_confidence_measure`) != 0)
        lapply(`id_confidence_measure`, function(x) stopifnot(R6::is.R6(x)))
        self$`id_confidence_measure` <- `id_confidence_measure`
      }
      if (!missing(`colunit-small_molecule`)) {
        stopifnot(is.list(`colunit-small_molecule`), length(`colunit-small_molecule`) != 0)
        lapply(`colunit-small_molecule`, function(x) stopifnot(R6::is.R6(x)))
        self$`colunit-small_molecule` <- `colunit-small_molecule`
      }
      if (!missing(`colunit-small_molecule_feature`)) {
        stopifnot(is.list(`colunit-small_molecule_feature`), length(`colunit-small_molecule_feature`) != 0)
        lapply(`colunit-small_molecule_feature`, function(x) stopifnot(R6::is.R6(x)))
        self$`colunit-small_molecule_feature` <- `colunit-small_molecule_feature`
      }
      if (!missing(`colunit-small_molecule_evidence`)) {
        stopifnot(is.list(`colunit-small_molecule_evidence`), length(`colunit-small_molecule_evidence`) != 0)
        lapply(`colunit-small_molecule_evidence`, function(x) stopifnot(R6::is.R6(x)))
        self$`colunit-small_molecule_evidence` <- `colunit-small_molecule_evidence`
      }
    },
    toJSON = function() {
      MetadataObject <- list()
      if (!is.null(self$`prefix`)) {
        MetadataObject[['prefix']] <- self$`prefix`
      }
      if (!is.null(self$`mzTab-version`)) {
        MetadataObject[['mzTab-version']] <- self$`mzTab-version`
      }
      if (!is.null(self$`mzTab-ID`)) {
        MetadataObject[['mzTab-ID']] <- self$`mzTab-ID`
      }
      if (!is.null(self$`title`)) {
        MetadataObject[['title']] <- self$`title`
      }
      if (!is.null(self$`description`)) {
        MetadataObject[['description']] <- self$`description`
      }
      if (!is.null(self$`sample_processing`)) {
        MetadataObject[['sample_processing']] <- lapply(self$`sample_processing`, function(x) x$toJSON())
      }
      if (!is.null(self$`instrument`)) {
        MetadataObject[['instrument']] <- lapply(self$`instrument`, function(x) x$toJSON())
      }
      if (!is.null(self$`software`)) {
        MetadataObject[['software']] <- lapply(self$`software`, function(x) x$toJSON())
      }
      if (!is.null(self$`publication`)) {
        MetadataObject[['publication']] <- lapply(self$`publication`, function(x) x$toJSON())
      }
      if (!is.null(self$`contact`)) {
        MetadataObject[['contact']] <- lapply(self$`contact`, function(x) x$toJSON())
      }
      if (!is.null(self$`uri`)) {
        MetadataObject[['uri']] <- lapply(self$`uri`, function(x) x$toJSON())
      }
      if (!is.null(self$`external_study_uri`)) {
        MetadataObject[['external_study_uri']] <- lapply(self$`external_study_uri`, function(x) x$toJSON())
      }
      if (!is.null(self$`quantification_method`)) {
        MetadataObject[['quantification_method']] <- self$`quantification_method`$toJSON()
      }
      if (!is.null(self$`study_variable`)) {
        MetadataObject[['study_variable']] <- lapply(self$`study_variable`, function(x) x$toJSON())
      }
      if (!is.null(self$`ms_run`)) {
        MetadataObject[['ms_run']] <- lapply(self$`ms_run`, function(x) x$toJSON())
      }
      if (!is.null(self$`assay`)) {
        MetadataObject[['assay']] <- lapply(self$`assay`, function(x) x$toJSON())
      }
      if (!is.null(self$`sample`)) {
        MetadataObject[['sample']] <- lapply(self$`sample`, function(x) x$toJSON())
      }
      if (!is.null(self$`custom`)) {
        MetadataObject[['custom']] <- lapply(self$`custom`, function(x) x$toJSON())
      }
      if (!is.null(self$`cv`)) {
        MetadataObject[['cv']] <- lapply(self$`cv`, function(x) x$toJSON())
      }
      if (!is.null(self$`database`)) {
        MetadataObject[['database']] <- lapply(self$`database`, function(x) x$toJSON())
      }
      if (!is.null(self$`derivatization_agent`)) {
        MetadataObject[['derivatization_agent']] <- lapply(self$`derivatization_agent`, function(x) x$toJSON())
      }
      if (!is.null(self$`small_molecule-quantification_unit`)) {
        MetadataObject[['small_molecule-quantification_unit']] <- self$`small_molecule-quantification_unit`$toJSON()
      }
      if (!is.null(self$`small_molecule_feature-quantification_unit`)) {
        MetadataObject[['small_molecule_feature-quantification_unit']] <- self$`small_molecule_feature-quantification_unit`$toJSON()
      }
      if (!is.null(self$`small_molecule-identification_reliability`)) {
        MetadataObject[['small_molecule-identification_reliability']] <- self$`small_molecule-identification_reliability`$toJSON()
      }
      if (!is.null(self$`id_confidence_measure`)) {
        MetadataObject[['id_confidence_measure']] <- lapply(self$`id_confidence_measure`, function(x) x$toJSON())
      }
      if (!is.null(self$`colunit-small_molecule`)) {
        MetadataObject[['colunit-small_molecule']] <- lapply(self$`colunit-small_molecule`, function(x) x$toJSON())
      }
      if (!is.null(self$`colunit-small_molecule_feature`)) {
        MetadataObject[['colunit-small_molecule_feature']] <- lapply(self$`colunit-small_molecule_feature`, function(x) x$toJSON())
      }
      if (!is.null(self$`colunit-small_molecule_evidence`)) {
        MetadataObject[['colunit-small_molecule_evidence']] <- lapply(self$`colunit-small_molecule_evidence`, function(x) x$toJSON())
      }

      MetadataObject
    },
    fromJSON = function(MetadataJson) {
      MetadataObject <- jsonlite::fromJSON(MetadataJson, simplifyVector = FALSE)
      if (!is.null(MetadataObject$`prefix`)) {
        self$`prefix` <- MetadataObject$`prefix`
      }
      if (!is.null(MetadataObject$`mzTab-version`)) {
        self$`mzTab-version` <- MetadataObject$`mzTab-version`
      }
      if (!is.null(MetadataObject$`mzTab-ID`)) {
        self$`mzTab-ID` <- MetadataObject$`mzTab-ID`
      }
      if (!is.null(MetadataObject$`title`)) {
        self$`title` <- MetadataObject$`title`
      }
      if (!is.null(MetadataObject$`description`)) {
        self$`description` <- MetadataObject$`description`
      }
      if (!is.null(MetadataObject$`sample_processing`)) {
        self$`sample_processing` <- lapply(MetadataObject$`sample_processing`, function(x) {
          `sample_processingObject` <- SampleProcessing$new()
          `sample_processingObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `sample_processingObject`
        })
      }
      if (!is.null(MetadataObject$`instrument`)) {
        self$`instrument` <- lapply(MetadataObject$`instrument`, function(x) {
          `instrumentObject` <- Instrument$new()
          `instrumentObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `instrumentObject`
        })
      }
      if (!is.null(MetadataObject$`software`)) {
        self$`software` <- lapply(MetadataObject$`software`, function(x) {
          `softwareObject` <- Software$new()
          `softwareObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `softwareObject`
        })
      }
      if (!is.null(MetadataObject$`publication`)) {
        self$`publication` <- lapply(MetadataObject$`publication`, function(x) {
          `publicationObject` <- Publication$new()
          `publicationObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `publicationObject`
        })
      }
      if (!is.null(MetadataObject$`contact`)) {
        self$`contact` <- lapply(MetadataObject$`contact`, function(x) {
          `contactObject` <- Contact$new()
          `contactObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `contactObject`
        })
      }
      if (!is.null(MetadataObject$`uri`)) {
        self$`uri` <- lapply(MetadataObject$`uri`, function(x) {
          `uriObject` <- Uri$new()
          `uriObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `uriObject`
        })
      }
      if (!is.null(MetadataObject$`external_study_uri`)) {
        self$`external_study_uri` <- lapply(MetadataObject$`external_study_uri`, function(x) {
          `external_study_uriObject` <- Uri$new()
          `external_study_uriObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `external_study_uriObject`
        })
      }
      if (!is.null(MetadataObject$`quantification_method`)) {
        `quantification_methodObject` <- Parameter$new()
        `quantification_methodObject`$fromJSON(jsonlite::toJSON(MetadataObject$quantification_method, auto_unbox = TRUE))
        self$`quantification_method` <- `quantification_methodObject`
      }
      if (!is.null(MetadataObject$`study_variable`)) {
        self$`study_variable` <- lapply(MetadataObject$`study_variable`, function(x) {
          `study_variableObject` <- StudyVariable$new()
          `study_variableObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `study_variableObject`
        })
      }
      if (!is.null(MetadataObject$`ms_run`)) {
        self$`ms_run` <- lapply(MetadataObject$`ms_run`, function(x) {
          `ms_runObject` <- MsRun$new()
          `ms_runObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `ms_runObject`
        })
      }
      if (!is.null(MetadataObject$`assay`)) {
        self$`assay` <- lapply(MetadataObject$`assay`, function(x) {
          `assayObject` <- Assay$new()
          `assayObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `assayObject`
        })
      }
      if (!is.null(MetadataObject$`sample`)) {
        self$`sample` <- lapply(MetadataObject$`sample`, function(x) {
          `sampleObject` <- Sample$new()
          `sampleObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `sampleObject`
        })
      }
      if (!is.null(MetadataObject$`custom`)) {
        self$`custom` <- lapply(MetadataObject$`custom`, function(x) {
          `customObject` <- Parameter$new()
          `customObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `customObject`
        })
      }
      if (!is.null(MetadataObject$`cv`)) {
        self$`cv` <- lapply(MetadataObject$`cv`, function(x) {
          `cvObject` <- CV$new()
          `cvObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `cvObject`
        })
      }
      if (!is.null(MetadataObject$`database`)) {
        self$`database` <- lapply(MetadataObject$`database`, function(x) {
          `databaseObject` <- Database$new()
          `databaseObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `databaseObject`
        })
      }
      if (!is.null(MetadataObject$`derivatization_agent`)) {
        self$`derivatization_agent` <- lapply(MetadataObject$`derivatization_agent`, function(x) {
          `derivatization_agentObject` <- Parameter$new()
          `derivatization_agentObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `derivatization_agentObject`
        })
      }
      if (!is.null(MetadataObject$`small_molecule-quantification_unit`)) {
        `small_molecule-quantification_unitObject` <- Parameter$new()
        print(MetadataObject$`small_molecule-quantification_unit`)
        jsonObj <- jsonlite::toJSON(MetadataObject$`small_molecule-quantification_unit`, auto_unbox = TRUE)
        #stop(jsonObj)
        `small_molecule-quantification_unitObject`$fromJSON(jsonObj)
        self$`small_molecule-quantification_unit` <- `small_molecule-quantification_unitObject`
      }
      if (!is.null(MetadataObject$`small_molecule_feature-quantification_unit`)) {
        `small_molecule_feature-quantification_unitObject` <- Parameter$new()
        `small_molecule_feature-quantification_unitObject`$fromJSON(jsonlite::toJSON(MetadataObject$`small_molecule_feature-quantification_unit`, auto_unbox = TRUE))
        self$`small_molecule_feature-quantification_unit` <- `small_molecule_feature-quantification_unitObject`
      }
      if (!is.null(MetadataObject$`small_molecule-identification_reliability`)) {
        `small_molecule-identification_reliabilityObject` <- Parameter$new()
        `small_molecule-identification_reliabilityObject`$fromJSON(jsonlite::toJSON(MetadataObject$`small_molecule-identification_reliability`, auto_unbox = TRUE))
        self$`small_molecule-identification_reliability` <- `small_molecule-identification_reliabilityObject`
      }
      if (!is.null(MetadataObject$`id_confidence_measure`)) {
        self$`id_confidence_measure` <- lapply(MetadataObject$`id_confidence_measure`, function(x) {
          `id_confidence_measureObject` <- Parameter$new()
          `id_confidence_measureObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `id_confidence_measureObject`
        })
      }
      if (!is.null(MetadataObject$`colunit-small_molecule`)) {
        self$`colunit-small_molecule` <- lapply(MetadataObject$`colunit-small_molecule`, function(x) {
          `colunit-small_moleculeObject` <- ColumnParameterMapping$new()
          `colunit-small_moleculeObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `colunit-small_moleculeObject`
        })
      }
      if (!is.null(MetadataObject$`colunit-small_molecule_feature`)) {
        self$`colunit-small_molecule_feature` <- lapply(MetadataObject$`colunit-small_molecule_feature`, function(x) {
          `colunit-small_molecule_featureObject` <- ColumnParameterMapping$new()
          `colunit-small_molecule_featureObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `colunit-small_molecule_featureObject`
        })
      }
      if (!is.null(MetadataObject$`colunit-small_molecule_evidence`)) {
        self$`colunit-small_molecule_evidence` <- lapply(MetadataObject$`colunit-small_molecule_evidence`, function(x) {
          `colunit-small_molecule_evidenceObject` <- ColumnParameterMapping$new()
          `colunit-small_molecule_evidenceObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `colunit-small_molecule_evidenceObject`
        })
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "prefix": %s,
           "mzTab-version": %s,
           "mzTab-ID": %s,
           "title": %s,
           "description": %s,
           "sample_processing": [%s],
           "instrument": [%s],
           "software": [%s],
           "publication": [%s],
           "contact": [%s],
           "uri": [%s],
           "external_study_uri": [%s],
           "quantification_method": %s,
           "study_variable": [%s],
           "ms_run": [%s],
           "assay": [%s],
           "sample": [%s],
           "custom": [%s],
           "cv": [%s],
           "database": [%s],
           "derivatization_agent": [%s],
           "small_molecule-quantification_unit": %s,
           "small_molecule_feature-quantification_unit": %s,
           "small_molecule-identification_reliability": %s,
           "id_confidence_measure": [%s],
           "colunit-small_molecule": [%s],
           "colunit-small_molecule_feature": [%s],
           "colunit-small_molecule_evidence": [%s]
        }',
        self$`prefix`,
        self$`mzTab-version`,
        self$`mzTab-ID`,
        self$`title`,
        self$`description`,
        lapply(self$`sample_processing`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`instrument`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`software`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`publication`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`contact`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`uri`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`external_study_uri`, function(x) paste(x$toJSON(), sep=",")),
        self$`quantification_method`$toJSON(),
        lapply(self$`study_variable`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`ms_run`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`assay`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`sample`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`custom`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`cv`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`database`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`derivatization_agent`, function(x) paste(x$toJSON(), sep=",")),
        self$`small_molecule-quantification_unit`$toJSON(),
        self$`small_molecule_feature-quantification_unit`$toJSON(),
        self$`small_molecule-identification_reliability`$toJSON(),
        lapply(self$`id_confidence_measure`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`colunit-small_molecule`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`colunit-small_molecule_feature`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`colunit-small_molecule_evidence`, function(x) paste(x$toJSON(), sep=","))
      )
    },
    fromJSONString = function(MetadataJson) {
      MetadataObject <- jsonlite::fromJSON(MetadataJson, simplifyVector = FALSE)
      self$`prefix` <- MetadataObject$`prefix`
      self$`mzTab-version` <- MetadataObject$`mzTab-version`
      self$`mzTab-ID` <- MetadataObject$`mzTab-ID`
      self$`title` <- MetadataObject$`title`
      self$`description` <- MetadataObject$`description`
      self$`sample_processing` <- lapply(MetadataObject$`sample_processing`, function(x) SampleProcessing$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`instrument` <- lapply(MetadataObject$`instrument`, function(x) Instrument$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`software` <- lapply(MetadataObject$`software`, function(x) Software$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`publication` <- lapply(MetadataObject$`publication`, function(x) Publication$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`contact` <- lapply(MetadataObject$`contact`, function(x) Contact$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`uri` <- lapply(MetadataObject$`uri`, function(x) Uri$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`external_study_uri` <- lapply(MetadataObject$`external_study_uri`, function(x) Uri$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      ParameterObject <- Parameter$new()
      self$`quantification_method` <- ParameterObject$fromJSON(jsonlite::toJSON(MetadataObject$quantification_method, auto_unbox = TRUE))
      self$`study_variable` <- lapply(MetadataObject$`study_variable`, function(x) StudyVariable$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`ms_run` <- lapply(MetadataObject$`ms_run`, function(x) MsRun$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`assay` <- lapply(MetadataObject$`assay`, function(x) Assay$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`sample` <- lapply(MetadataObject$`sample`, function(x) Sample$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`custom` <- lapply(MetadataObject$`custom`, function(x) Parameter$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`cv` <- lapply(MetadataObject$`cv`, function(x) CV$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`database` <- lapply(MetadataObject$`database`, function(x) Database$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`derivatization_agent` <- lapply(MetadataObject$`derivatization_agent`, function(x) Parameter$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      ParameterObject <- Parameter$new()
      self$`small_molecule-quantification_unit` <- ParameterObject$fromJSON(jsonlite::toJSON(MetadataObject$small_molecule-quantification_unit, auto_unbox = TRUE))
      ParameterObject <- Parameter$new()
      self$`small_molecule_feature-quantification_unit` <- ParameterObject$fromJSON(jsonlite::toJSON(MetadataObject$small_molecule_feature-quantification_unit, auto_unbox = TRUE))
      ParameterObject <- Parameter$new()
      self$`small_molecule-identification_reliability` <- ParameterObject$fromJSON(jsonlite::toJSON(MetadataObject$small_molecule-identification_reliability, auto_unbox = TRUE))
      self$`id_confidence_measure` <- lapply(MetadataObject$`id_confidence_measure`, function(x) Parameter$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`colunit-small_molecule` <- lapply(MetadataObject$`colunit-small_molecule`, function(x) ColumnParameterMapping$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`colunit-small_molecule_feature` <- lapply(MetadataObject$`colunit-small_molecule_feature`, function(x) ColumnParameterMapping$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`colunit-small_molecule_evidence` <- lapply(MetadataObject$`colunit-small_molecule_evidence`, function(x) ColumnParameterMapping$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
    }
  )
)