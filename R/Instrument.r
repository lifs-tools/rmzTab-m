# mzTab-M reference implementation and validation API.
# 
# This is the mzTab-M reference implementation and validation API service.
# 
# OpenAPI spec version: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://github.com/swagger-api/swagger-codegen.git


#' Instrument Class
#'
#' The name, source, analyzer and detector of the instruments used in the experiment. Multiple instruments are numbered [1-n].
#'
#' @field id 
#' @field elementType 
#' @field name 
#' @field source 
#' @field analyzer 
#' @field detector 
#'
#' mzTab-M specification example(s)
#' \preformatted{
#' MTD	instrument[1]-name	[MS, MS:1000449, LTQ Orbitrap,]
#' MTD	instrument[1]-source	[MS, MS:1000073, ESI,]
#' …
#' MTD	instrument[2]-source	[MS, MS:1000598, ETD,]
#' MTD	instrument[1]-analyzer[1]	[MS, MS:1000291, linear ion trap,]
#' …
#' MTD	instrument[2]-analyzer[1]	[MS, MS:1000484, orbitrap,]
#' MTD	instrument[1]-detector	[MS, MS:1000253, electron multiplier,]
#' …
#' MTD	instrument[2]-detector	[MS, MS:1000348, focal plane collector,]
#' 
#' }
#' 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Instrument <- R6::R6Class(
  'Instrument',
  public = list(
    `id` = NULL,
    `elementType` = NULL,
    `name` = NULL,
    `source` = NULL,
    `analyzer` = NULL,
    `detector` = NULL,
    initialize = function(`id`, `elementType`, `name`, `source`, `analyzer`, `detector`){
      if (!missing(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!missing(`elementType`)) {
        stopifnot(is.character(`elementType`), length(`elementType`) == 1)
        self$`elementType` <- `elementType`
      }
      if (!missing(`name`)) {
        stopifnot(R6::is.R6(`name`))
        self$`name` <- `name`
      }
      if (!missing(`source`)) {
        stopifnot(R6::is.R6(`source`))
        self$`source` <- `source`
      }
      if (!missing(`analyzer`)) {
        stopifnot(is.list(`analyzer`), length(`analyzer`) != 0)
        lapply(`analyzer`, function(x) stopifnot(R6::is.R6(x)))
        self$`analyzer` <- `analyzer`
      }
      if (!missing(`detector`)) {
        stopifnot(R6::is.R6(`detector`))
        self$`detector` <- `detector`
      }
    },
    toJSON = function() {
      InstrumentObject <- list()
      if (!is.null(self$`id`)) {
        InstrumentObject[['id']] <- self$`id`
      }
      if (!is.null(self$`elementType`)) {
        InstrumentObject[['elementType']] <- self$`elementType`
      }
      if (!is.null(self$`name`)) {
        InstrumentObject[['name']] <- self$`name`$toJSON()
      }
      if (!is.null(self$`source`)) {
        InstrumentObject[['source']] <- self$`source`$toJSON()
      }
      if (!is.null(self$`analyzer`)) {
        InstrumentObject[['analyzer']] <- lapply(self$`analyzer`, function(x) x$toJSON())
      }
      if (!is.null(self$`detector`)) {
        InstrumentObject[['detector']] <- self$`detector`$toJSON()
      }

      InstrumentObject
    },
    fromJSON = function(InstrumentJson) {
      InstrumentObject <- jsonlite::fromJSON(InstrumentJson, simplifyVector = FALSE)
      if (!is.null(InstrumentObject$`id`)) {
        self$`id` <- InstrumentObject$`id`
      }
      if (!is.null(InstrumentObject$`elementType`)) {
        self$`elementType` <- InstrumentObject$`elementType`
      }
      if (!is.null(InstrumentObject$`name`)) {
        `nameObject` <- Parameter$new()
        `nameObject`$fromJSON(jsonlite::toJSON(InstrumentObject$name, auto_unbox = TRUE))
        self$`name` <- `nameObject`
      }
      if (!is.null(InstrumentObject$`source`)) {
        `sourceObject` <- Parameter$new()
        `sourceObject`$fromJSON(jsonlite::toJSON(InstrumentObject$source, auto_unbox = TRUE))
        self$`source` <- `sourceObject`
      }
      if (!is.null(InstrumentObject$`analyzer`)) {
        self$`analyzer` <- lapply(InstrumentObject$`analyzer`, function(x) {
          `analyzerObject` <- Parameter$new()
          `analyzerObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `analyzerObject`
        })
      }
      if (!is.null(InstrumentObject$`detector`)) {
        `detectorObject` <- Parameter$new()
        `detectorObject`$fromJSON(jsonlite::toJSON(InstrumentObject$detector, auto_unbox = TRUE))
        self$`detector` <- `detectorObject`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "elementType": %s,
           "name": %s,
           "source": %s,
           "analyzer": [%s],
           "detector": %s
        }',
        self$`id`,
        self$`elementType`,
        self$`name`$toJSON(),
        self$`source`$toJSON(),
        lapply(self$`analyzer`, function(x) paste(x$toJSON(), sep=",")),
        self$`detector`$toJSON()
      )
    },
    fromJSONString = function(InstrumentJson) {
      InstrumentObject <- jsonlite::fromJSON(InstrumentJson, simplifyVector = FALSE)
      self$`id` <- InstrumentObject$`id`
      self$`elementType` <- InstrumentObject$`elementType`
      ParameterObject <- Parameter$new()
      self$`name` <- ParameterObject$fromJSON(jsonlite::toJSON(InstrumentObject$name, auto_unbox = TRUE))
      ParameterObject <- Parameter$new()
      self$`source` <- ParameterObject$fromJSON(jsonlite::toJSON(InstrumentObject$source, auto_unbox = TRUE))
      self$`analyzer` <- lapply(InstrumentObject$`analyzer`, function(x) Parameter$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      ParameterObject <- Parameter$new()
      self$`detector` <- ParameterObject$fromJSON(jsonlite::toJSON(InstrumentObject$detector, auto_unbox = TRUE))
    }
  )
)