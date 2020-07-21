#'
#'This method adds a default value to return if its argument is null in comparison to rmzTabM::safe_unbox.
#'
#'@param x
#'@param default the default value to return, if x is null
#'@export
safe_unbox <- function(x, default = NULL) {
  if (!is.atomic(x)) {
    if (is.null(x)) {
      return(x)
    }  
  } 
  if (is.data.frame(x)) {
    if (nrow(x) == 1) {
      return(jsonlite:::as.scalar(x))
    }
    else {
      stop("Tried to unbox dataframe with ", nrow(x), 
           " rows.")
    }
  }
  if (is.null(x)) {
    #print(paste("Returning default value", default))
    return(default)
  } 
  if (is.list(x)) {
   return(unlist(x))
  }
  if (!is.atomic(x) || length(dim(x)) > 1) {
    print(paste(x, "is not atomic!"))
    stop("Only atomic vectors of length 1 or data frames with 1 row can be unboxed.")
  }
  if (identical(length(x), 1L)) {
    return(jsonlite:::as.scalar(x))
  }
  else {
    stop("Tried to unbox a vector of length ", length(x))
  }
}
