# mzTab-M reference implementation and validation API.
# 
# This is the mzTab-M reference implementation and validation API service.
# 
# OpenAPI spec version: 2.0.0
# Contact: nils.hoffmann@isas.de
# Generated by: https://github.com/swagger-api/swagger-codegen.git


#' Publication Class
#'
#' A publication associated with this file. Several publications can be given by indicating the number in the square brackets after “publication”. PubMed ids must be prefixed by “pubmed:”, DOIs by “doi:”. Multiple identifiers MUST be separated by “|”. 
#'
#' @field id 
#' @field elementType 
#' @field publicationItems 
#'
#' mzTab-M specification example(s)
#' \preformatted{
#' MTD	publication[1]	pubmed:21063943|doi:10.1007/978-1-60761-987-1_6
#' MTD	publication[2]	pubmed:20615486|doi:10.1016/j.jprot.2010.06.008
#' 
#' }
#' 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Publication <- R6::R6Class(
  'Publication',
  public = list(
    `id` = NULL,
    `elementType` = NULL,
    `publicationItems` = NULL,
    initialize = function(`id`, `elementType`, `publicationItems`){
      if (!missing(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!missing(`elementType`)) {
        stopifnot(is.character(`elementType`), length(`elementType`) == 1)
        self$`elementType` <- `elementType`
      }
      if (!missing(`publicationItems`)) {
        stopifnot(is.list(`publicationItems`), length(`publicationItems`) != 0)
        lapply(`publicationItems`, function(x) stopifnot(R6::is.R6(x)))
        self$`publicationItems` <- `publicationItems`
      }
    },
    toJSON = function() {
      PublicationObject <- list()
      if (!is.null(self$`id`)) {
        PublicationObject[['id']] <- self$`id`
      }
      if (!is.null(self$`elementType`)) {
        PublicationObject[['elementType']] <- self$`elementType`
      }
      if (!is.null(self$`publicationItems`)) {
        PublicationObject[['publicationItems']] <- lapply(self$`publicationItems`, function(x) x$toJSON())
      }

      PublicationObject
    },
    fromJSON = function(PublicationJson) {
      PublicationObject <- jsonlite::fromJSON(PublicationJson, simplifyVector = FALSE)
      if (!is.null(PublicationObject$`id`)) {
        self$`id` <- PublicationObject$`id`
      }
      if (!is.null(PublicationObject$`elementType`)) {
        self$`elementType` <- PublicationObject$`elementType`
      }
      if (!is.null(PublicationObject$`publicationItems`)) {
        self$`publicationItems` <- lapply(PublicationObject$`publicationItems`, function(x) {
          `publicationItemsObject` <- PublicationItem$new()
          `publicationItemsObject`$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          `publicationItemsObject`
        })
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "elementType": %s,
           "publicationItems": [%s]
        }',
        self$`id`,
        self$`elementType`,
        lapply(self$`publicationItems`, function(x) paste(x$toJSON(), sep=","))
      )
    },
    fromJSONString = function(PublicationJson) {
      PublicationObject <- jsonlite::fromJSON(PublicationJson, simplifyVector = FALSE)
      self$`id` <- PublicationObject$`id`
      self$`elementType` <- PublicationObject$`elementType`
      self$`publicationItems` <- lapply(PublicationObject$`publicationItems`, function(x) PublicationItem$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
    }
  )
)
