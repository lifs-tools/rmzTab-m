#'
#'This method adds a default value to return if its argument is null in comparison to rmzTabM::safe_unbox.
#'
#'@param x the list, vector, or array object to unbox into a singleton
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
      return(as.scalar(x))
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
    return(as.scalar(x))
  }
  else {
    stop("Tried to unbox a vector of length ", length(x))
  }
}

# This function is originally from the jsonlite package, file as.scalar.R
# Unfortunately, that function is not exported from jsonlite, but we need it for 
# our more safe_unbox function above. The call to is.namedlist has been 
# inlined with the actual code.
as.scalar <- function(obj) {
  # Lists can never be a scalar (this can arise if a dataframe contains a column
  # with lists)
  if(length(dim(obj)) > 1){
    if(!identical(nrow(obj), 1L)){
      warning("Tried to use as.scalar on an array or dataframe with ", nrow(obj), " rows.", call.=FALSE)
      return(obj)
    }
  } else if(!identical(length(obj), 1L)) {
    warning("Tried to use as.scalar on an object of length ", length(obj), call.=FALSE)
    return(obj)
  } else if(isTRUE(is.list(obj) && !is.null(names(obj)))){
    warning("Tried to use as.scalar on a named list.", call.=FALSE)
    return(obj)
  }
  
  class(obj) <- c("scalar", class(obj))
  return(obj)
}
