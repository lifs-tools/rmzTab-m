# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@cebitec.uni-bielefeld.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title Publication
#' @description Publication Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#'
#' @field publicationItems  list( \link{PublicationItem} ) 
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Publication <- R6::R6Class(
  'Publication',
  public = list(
    `id` = NULL,
    `publicationItems` = NULL,
    #' @description Create a Publication
    #' @param publicationItems The list( \link{PublicationItem} )s.
    #' @param id Publication id.
    #' @param ... local optional variable arguments
    #' 
    initialize = function(`publicationItems`, `id`=NULL, ...){
      local.optional.var <- list(...)
      if (!missing(`publicationItems`)) {
        stopifnot(is.vector(`publicationItems`), length(`publicationItems`) != 0)
        sapply(`publicationItems`, function(x) stopifnot(R6::is.R6(x)))
        self$`publicationItems` <- `publicationItems`
      }
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
    },
    #' @description Serialize to list object suitable for jsonlite    
    toJSON = function() {
      PublicationObject <- list()
      if (!is.null(self$`id`)) {
        PublicationObject[['id']] <-
          rmzTabM::safe_unbox(self$`id`)
      }
      if (!is.null(self$`publicationItems`)) {
        PublicationObject[['publicationItems']] <-
          lapply(self$`publicationItems`, function(x) x$toJSON())
      }

      PublicationObject
    },
    #' @description Deserialize from jsonlite list object
    #' @param PublicationJson list object.
    fromJSON = function(PublicationJson) {
      PublicationObject <- jsonlite::fromJSON(PublicationJson)
      if (!is.null(PublicationObject$`id`)) {
        self$`id` <- PublicationObject$`id`
      }
      if (!is.null(PublicationObject$`publicationItems`)) {
        self$`publicationItems` <- ApiClient$new()$deserializeObj(PublicationObject$`publicationItems`, "array[PublicationItem]", loadNamespace("rmzTabM"))
      }
    },
    #' @description Serialize to JSON string.    
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
        sprintf(
        '"id":
          %d
                ',
        rmzTabM::safe_unbox(self$`id`)
        )},
        if (!is.null(self$`publicationItems`)) {
        sprintf(
        '"publicationItems":
        %s
',
        paste(sapply(self$`publicationItems`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)), collapse=",")
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    #' @description Deserialize from JSON string
    #' @param PublicationJson JSON string    
    fromJSONString = function(PublicationJson) {
      PublicationObject <- jsonlite::fromJSON(PublicationJson)
      self$`id` <- PublicationObject$`id`
      self$`publicationItems` <- ApiClient$new()$deserializeObj(PublicationObject$`publicationItems`, "array[PublicationItem]", loadNamespace("rmzTabM"))
      self
    },
    #' @description Serialize to data frame
    toDataFrame = function() {
      idPrefix <- paste0("publication[", self$`id`, "]")
      elements <- data.frame(PREFIX=character(), KEY=character(), VALUE=character())
      if (!is.null(self$`publicationItems`)) {
        elements <-
          rbind(elements,
                list(
                  PREFIX = "MTD", 
                  KEY=idPrefix, 
                  VALUE=paste0(lapply(self$`publicationItems`, function(x) x$toString()), collapse="|")
                  ),
                stringsAsFactors = FALSE)
      }
      elements
    },
    #' @description Deserialize from publication data frame
    #' @param PublicationDataFrame Publication data frame
    fromDataFrame = function(PublicationDataFrame) {
      if (rlang::has_name(PublicationDataFrame, "id")) {
        self$`id` <- PublicationDataFrame$`id`
      }
      if (rlang::has_name(PublicationDataFrame, "name")) {
        pubItemsList <- splitList(PublicationDataFrame$`name`)
        self$`publicationItems` <- lapply(pubItemsList, function(x){
          pubItem <- PublicationItem$new()
          pubItem$fromString(x)
          pubItem
        })
      }
      self
    }
  )
)
