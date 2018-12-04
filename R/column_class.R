#' Returns all columns and classes matching specified class
#' 
#' The \code{col_class} function takes two arguments, \code{x}, a data.frame
#' object, and \code{find_class}, a single or vector of character value(s) of
#' the class type desired.
#' 
#' @param x a data.frame object with names columns
#' @param find_class a single or vector of character value(s) of column class
#'   types
#' @examples
#' x <- data.frame(
#'   col_a = as.factor(c("a", "b", "c"))
#'   , col_b = as.integer(c(1, 2, 3))
#'   , col_c = as.factor(c("apple", "banana", "citrus"))
#'   , col_d = as.character(c("one", "two", "three"))
#' )
#' col_class(x, "factor")
#' col_class(x, c("integer", "character"))
#' @export
col_class <- function(x, find_class = NULL) {
  if (is.null(colnames(x))) {
    stop(
      paste0(
        "\nObject "
        , deparse(substitute(x))
        , " does not contain named columns.\n"
        )
      )
  } else if (is.null(find_class)) {
    #return all column names and classes
    c <- sapply(x, class)
  } else {
    # return all column names and classes where class matches
    c <- sapply(x, class)
    if (length(find_class) > 1) {
    c <- c[c %in% find_class]
    } else {
      c <- c[c == find_class]
    }
  }
  return(c)
}
