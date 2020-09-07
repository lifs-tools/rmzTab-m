#'
#' Check if x is null. If so, return defaultValue, otherwise run function on x
#' and return the result. If a value x is encountered that is NULL, and nullable is set to FALSE,
#' execution will be stopped and an error message will be printed.
#' 
#' @param x the argument to create a value for.
#' @param FUN the function to apply to x, if x is not NULL.
#' @param nullable if TRUE, NULL is a valid value, otherwise, an error will be raised.
#' @param defaultValue the value to return, if x is NULL.
#' @param ... arguments that will be passed on to FUN.
#' @export
valueOrDefault <- function(x, FUN = identity, nullable = TRUE, defaultValue="null", ...) {
  if (is.null(x)) {
    if (nullable) {
      return(defaultValue)
    } else {
      stop("Value is not nullable, but was NULL!")
    }
  } else {
    return(FUN(x, ...))
  }
}

## utility function, combining different length objects into a dataframe
## padding short columns with NA
rbind.ragged <- function(x, y) {
  x <- as.data.frame(x) 
  y <- as.data.frame(y) 
  colnames(x) <- seq(1:ncol(x))
  colnames(y) <- seq(1:ncol(y))
  dplyr::bind_rows(x,y)
}
