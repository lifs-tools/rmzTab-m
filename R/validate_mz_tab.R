#'
#' Validate the provided mztab object against the online mzTab-M validator.
#' @param mztab the R6 mzTab object to write.
#' @param validationMode if "json", validate against the mzTab-M validator online service by converting the mzTab data model to JSON,
#' if "plain", validate against the mzTab-M validator online service by converting the mzTab data model to TSV format before sending.
#' @param validationLevel level of validation messages to return, can be one of 'info', 'warn' or 'error'.
#' @param maxErrors maximum number of validation errors at which the validation should stop.
#' @param semanticValidation use semantic validation (CV parameters) against the default mapping file.
#' @return a list of validation messages or an empty list if no messages were generated.
#' @export
validateMzTab <-
  function(mztab,
           validationMode = "json",
           validationLevel = "info",
           maxErrors = 100,
           semanticValidation = TRUE) {
    # set a custom api client to use a different URL
    apiClient <-
      ApiClient$new(basePath = "https://apps.lifs.isas.de/mztabvalidator-dev/rest/v2")
    if (validationMode == "json") {
      stopifnot(R6::is.R6(mztab))
      stopifnot("MzTab" != mztab$classname)
      validateApi <- ValidateApi$new(apiClient = apiClient)
      response <-
        validateApi$ValidateMzTabFile(mztab, validationLevel, maxErrors, semanticValidation)
      
      if (!is.null(response$response)) {
        # retrieve the validation messages
        validationMessages <-
          apiClient$deserialize(resp = response$response,
                                returnType = "array[ValidationMessage]",
                                loadNamespace("rmzTabM"))
        lapply(validationMessages, function(x) {
          if (x$message_type != 'info') {
            warning(
              paste0(
                "[",
                x$code,
                ", type=",
                x$message_type,
                ", category=",
                x$category,
                ", message=",
                x$message,
                ", line=",
                x$line_number,
                "]"
              )
            )
          } else {
            message(
              paste0(
                "[",
                x$code,
                ", type=",
                x$message_type,
                ", category=",
                x$category,
                ", message=",
                x$message,
                ", line=",
                x$line_number,
                "]"
              )
            )
          }
        })
        return(validationMessages)
      } else {
        return(list())
      }
    } else if (validationMode == "plain") {
      validatePlainApi <- ValidatePlainApi$new(apiClient = apiClient)
      tmpFile <- tempfile(fileext = "mztab")
      # writeMzTab(mztab, tmpFile)
      # mzTabString <- readChar(tmpFile, file.info(tmpFile)$size)
      response <-
        validatePlainApi$ValidatePlainMzTabFile(mztab,
                                                validationLevel,
                                                maxErrors,
                                                semanticValidation)
      
      if (!is.null(response$response)) {
        # retrieve the validation messages
        validationMessages <-
          apiClient$deserialize(resp = response$response,
                                returnType = "array[ValidationMessage]",
                                loadNamespace("rmzTabM"))
        lapply(validationMessages, function(x) {
          if (x$message_type != 'info') {
            warning(
              paste0(
                "[",
                x$code,
                ", type=",
                x$message_type,
                ", category=",
                x$category,
                ", message=",
                x$message,
                ", line=",
                x$line_number,
                "]"
              )
            )
          } else {
            message(
              paste0(
                "[",
                x$code,
                ", type=",
                x$message_type,
                ", category=",
                x$category,
                ", message=",
                x$message,
                ", line=",
                x$line_number,
                "]"
              )
            )
          }
        })
        return(validationMessages)
      } else {
        return(list())
      }
    } else {
      stop(
        paste(
          "Unknown validationMode=",
          validationMode,
          "! Supported values are 'json' or 'plain'!"
        )
      )
    }
  }
