# mzTab-M reference implementation and validation API.
# 
# This is the mzTab-M reference implementation and validation API service.
# 
# OpenAPI spec version: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://github.com/swagger-api/swagger-codegen.git


#' Software Class
#'
#' Software used to analyze the data and obtain the reported results. The parameter’s value SHOULD contain the software’s version. The order (numbering) should reflect the order in which the tools were used. A software setting used. This field MAY occur multiple times for a single software. The value of this field is deliberately set as a String, since there currently do not exist CV terms for every possible setting. 
#'
#' @field id 
#' @field elementType 
#' @field parameter 
#' @field setting 
#'
#' mzTab-M specification example(s)
#' \preformatted{
#' MTD	software[1]	[MS, MS:1002879, Progenesis QI, 3.0]
#' MTD	software[1]-setting	Fragment tolerance = 0.1 Da
#' …
#' MTD	software[2]-setting	Parent tolerance = 0.5 Da
#' 
#' }
#' 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Software <- R6::R6Class(
  'Software',
  public = list(
    `id` = NULL,
    `elementType` = NULL,
    `parameter` = NULL,
    `setting` = NULL,
    initialize = function(`id`, `elementType`, `parameter`, `setting`){
      if (!missing(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!missing(`elementType`)) {
        stopifnot(is.character(`elementType`), length(`elementType`) == 1)
        self$`elementType` <- `elementType`
      }
      if (!missing(`parameter`)) {
        stopifnot(R6::is.R6(`parameter`))
        self$`parameter` <- `parameter`
      }
      if (!missing(`setting`)) {
        stopifnot(is.list(`setting`), length(`setting`) != 0)
        lapply(`setting`, function(x) stopifnot(is.character(x)))
        self$`setting` <- `setting`
      }
    },
    toJSON = function() {
      SoftwareObject <- list()
      if (!is.null(self$`id`)) {
        SoftwareObject[['id']] <- self$`id`
      }
      if (!is.null(self$`elementType`)) {
        SoftwareObject[['elementType']] <- self$`elementType`
      }
      if (!is.null(self$`parameter`)) {
        SoftwareObject[['parameter']] <- self$`parameter`$toJSON()
      }
      if (!is.null(self$`setting`)) {
        SoftwareObject[['setting']] <- self$`setting`
      }

      SoftwareObject
    },
    fromJSON = function(SoftwareJson) {
      SoftwareObject <- jsonlite::fromJSON(SoftwareJson, simplifyVector = FALSE)
      if (!is.null(SoftwareObject$`id`)) {
        self$`id` <- SoftwareObject$`id`
      }
      if (!is.null(SoftwareObject$`elementType`)) {
        self$`elementType` <- SoftwareObject$`elementType`
      }
      if (!is.null(SoftwareObject$`parameter`)) {
        `parameterObject` <- Parameter$new()
        `parameterObject`$fromJSON(jsonlite::toJSON(SoftwareObject$parameter, auto_unbox = TRUE))
        self$`parameter` <- `parameterObject`
      }
      if (!is.null(SoftwareObject$`setting`)) {
        self$`setting` <- SoftwareObject$`setting`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "elementType": %s,
           "parameter": %s,
           "setting": [%s]
        }',
        self$`id`,
        self$`elementType`,
        self$`parameter`$toJSON(),
        lapply(self$`setting`, function(x) paste(paste0('"', x, '"'), sep=","))
      )
    },
    fromJSONString = function(SoftwareJson) {
      SoftwareObject <- jsonlite::fromJSON(SoftwareJson, simplifyVector = FALSE)
      self$`id` <- SoftwareObject$`id`
      self$`elementType` <- SoftwareObject$`elementType`
      ParameterObject <- Parameter$new()
      self$`parameter` <- ParameterObject$fromJSON(jsonlite::toJSON(SoftwareObject$parameter, auto_unbox = TRUE))
      self$`setting` <- SoftwareObject$`setting`
    }
  )
)
