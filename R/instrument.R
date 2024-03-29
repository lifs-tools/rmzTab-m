# mzTab-M reference implementation and validation API.
#
# This is the mzTab-M reference implementation and validation API service.
#
# The version of the OpenAPI document: 2.0.0
# Contact: nils.hoffmann@cebitec.uni-bielefeld.de
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title Instrument
#' @description Instrument Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#'
#' @field name  \link{Parameter} [optional]
#'
#' @field source  \link{Parameter} [optional]
#'
#' @field analyzer  list( \link{Parameter} ) [optional]
#'
#' @field detector  \link{Parameter} [optional]
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Instrument <- R6::R6Class(
  'Instrument',
  public = list(
    `id` = NULL,
    `name` = NULL,
    `source` = NULL,
    `analyzer` = NULL,
    `detector` = NULL,
    #' @description Create an Instrument
    #' @param id Instrument id.
    #' @param name Instrument name \link{Parameter}.
    #' @param source Instrument source \link{Parameter}.
    #' @param analyzer Instrument analyzer list( \link{Parameter} )s.
    #' @param detector Instrument detector \link{Parameter}.
    #' @param ... local optional variable arguments
    #' 
    initialize = function(`id`=NULL, `name`=NULL, `source`=NULL, `analyzer`=NULL, `detector`=NULL, ...){
      local.optional.var <- list(...)
      if (!is.null(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`name`)) {
        stopifnot(R6::is.R6(`name`))
        self$`name` <- `name`
      }
      if (!is.null(`source`)) {
        stopifnot(R6::is.R6(`source`))
        self$`source` <- `source`
      }
      if (!is.null(`analyzer`)) {
        stopifnot(is.vector(`analyzer`), length(`analyzer`) != 0)
        sapply(`analyzer`, function(x) stopifnot(R6::is.R6(x)))
        self$`analyzer` <- `analyzer`
      }
      if (!is.null(`detector`)) {
        stopifnot(R6::is.R6(`detector`))
        self$`detector` <- `detector`
      }
    },
    #' @description Serialize to list object suitable for jsonlite
    toJSON = function() {
      InstrumentObject <- list()
      if (!is.null(self$`id`)) {
        InstrumentObject[['id']] <-
          rmzTabM::safe_unbox(self$`id`)
      }
      if (!is.null(self$`name`)) {
        InstrumentObject[['name']] <-
          self$`name`$toJSON()
      }
      if (!is.null(self$`source`)) {
        InstrumentObject[['source']] <-
          self$`source`$toJSON()
      }
      if (!is.null(self$`analyzer`)) {
        InstrumentObject[['analyzer']] <-
          lapply(self$`analyzer`, function(x) x$toJSON())
      }
      if (!is.null(self$`detector`)) {
        InstrumentObject[['detector']] <-
          self$`detector`$toJSON()
      }

      InstrumentObject
    },
    #' @description Deserialize from jsonlite list object
    #' @param InstrumentJson list object.
    fromJSON = function(InstrumentJson) {
      InstrumentObject <- jsonlite::fromJSON(InstrumentJson)
      if (!is.null(InstrumentObject$`id`)) {
        self$`id` <- InstrumentObject$`id`
      }
      if (!is.null(InstrumentObject$`name`)) {
        nameObject <- Parameter$new()
        nameObject$fromJSON(jsonlite::toJSON(InstrumentObject$name, auto_unbox = FALSE, null = "null", na = "null", digits = NA))
        self$`name` <- nameObject
      }
      if (!is.null(InstrumentObject$`source`)) {
        sourceObject <- Parameter$new()
        sourceObject$fromJSON(jsonlite::toJSON(InstrumentObject$source, auto_unbox = FALSE, null = "null", na = "null", digits = NA))
        self$`source` <- sourceObject
      }
      if (!is.null(InstrumentObject$`analyzer`)) {
        self$`analyzer` <- ApiClient$new()$deserializeObj(InstrumentObject$`analyzer`, "array[Parameter]", loadNamespace("rmzTabM"))
      }
      if (!is.null(InstrumentObject$`detector`)) {
        detectorObject <- Parameter$new()
        detectorObject$fromJSON(jsonlite::toJSON(InstrumentObject$detector, auto_unbox = FALSE, null = "null", na = "null", digits = NA))
        self$`detector` <- detectorObject
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
        if (!is.null(self$`name`)) {
        sprintf(
        '"name":
        %s
        ',
        jsonlite::toJSON(self$`name`$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)
        )},
        if (!is.null(self$`source`)) {
        sprintf(
        '"source":
        %s
        ',
        jsonlite::toJSON(self$`source`$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)
        )},
        if (!is.null(self$`analyzer`)) {
        sprintf(
        '"analyzer":
        [%s]
',
        paste(sapply(self$`analyzer`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)), collapse=",")
        )},
        if (!is.null(self$`detector`)) {
        sprintf(
        '"detector":
        %s
        ',
        jsonlite::toJSON(self$`detector`$toJSON(), auto_unbox=FALSE, null = "null", na = "null", digits = NA)
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    #' @description Deserialize from JSON string
    #' @param InstrumentJson JSON string
    fromJSONString = function(InstrumentJson) {
      InstrumentObject <- jsonlite::fromJSON(InstrumentJson)
      self$`id` <- InstrumentObject$`id`
      self$`name` <- Parameter$new()$fromJSONString(jsonlite::toJSON(InstrumentObject$name, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
      self$`source` <- Parameter$new()$fromJSONString(jsonlite::toJSON(InstrumentObject$source, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
      self$`analyzer` <- ApiClient$new()$deserializeObj(InstrumentObject$`analyzer`, "array[Parameter]", loadNamespace("rmzTabM"))
      self$`detector` <- Parameter$new()$fromJSONString(jsonlite::toJSON(InstrumentObject$detector, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
      self
    },
    #' @description Serialize to data frame
    toDataFrame = function() {
      idPrefix <- paste0("instrument[", self$`id`, "]")
      elements <- data.frame(PREFIX=character(), KEY=character(), VALUE=character())
      if (!is.null(self$`name`)) {
        elements <-
          rbind(elements,
                list(
                  PREFIX = "MTD",
                  KEY = paste(idPrefix, "name", sep = "-"),
                  VALUE = self$`name`$toString()
                ),
                stringsAsFactors = FALSE)
      }
      if (!is.null(self$`source`)) {
        elements <-
          rbind(elements,
                list(
                  PREFIX = "MTD",
                  KEY = paste(idPrefix, "source", sep = "-"),
                  VALUE = self$`source`$toString()
                ),
                stringsAsFactors = FALSE)
      }
      if (!is.null(self$`analyzer`)) {
        elements <-
          rbind(elements,
                lapply(seq_along(self$`analyzer`), function(idx, elements, idPrefix) {
                  list(PREFIX = "MTD", KEY=paste(idPrefix, paste0("analyzer[", idx, "]"), sep="-"), VALUE=elements[[idx]]$toString())
                }, elements=self$`analyzer`, idPrefix=idPrefix) %>% dplyr::bind_rows(),
                stringsAsFactors = FALSE)
      }
      if (!is.null(self$`detector`)) {
        elements <-
          rbind(elements,
                list(
                  PREFIX = "MTD",
                  KEY = paste(idPrefix, "detector", sep = "-"),
                  VALUE = self$`detector`$toString()
                ),
                stringsAsFactors = FALSE)
      }
      elements
    },
    #' @description Deserialize from instrument data frame
    #' @param InstrumentDataFrame Instrument data frame
    fromDataFrame = function(InstrumentDataFrame) {
      stopifnot(nrow(InstrumentDataFrame)==1)
      columnNames <- colnames(InstrumentDataFrame)
      if (rlang::has_name(InstrumentDataFrame, "id")) {
        self$`id` <- as.numeric(InstrumentDataFrame$`id`)
      }
      if (rlang::has_name(InstrumentDataFrame, "name")) {
        param <- Parameter$new()
        self$`name` <- param$fromString(NULL, InstrumentDataFrame$`name`)
      }
      if (rlang::has_name(InstrumentDataFrame, "source")) {
        param <- Parameter$new()
        self$`source` <- param$fromString(NULL, InstrumentDataFrame$`source`)
      }
      # extract potentially multiple columns with 'analyzer' prefix
      analyzerColumns <- columnNames[grepl("^analyzer", columnNames)]
      if (length(analyzerColumns) > 0) {
        self$`analyzer` <- lapply(analyzerColumns, function(x) {
          param <- Parameter$new()
          param$fromString(NULL, InstrumentDataFrame[[x]])
          param
        })
      }
      if (rlang::has_name(InstrumentDataFrame, "detector")) {
        param <- Parameter$new()
        self$`detector` <- param$fromString(NULL, InstrumentDataFrame$`detector`)
      }
      self
    }
  )
)
