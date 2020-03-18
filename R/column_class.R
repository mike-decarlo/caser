#' @title Returns all columns and classes matching specified class
#'
#' @description The \code{column_class} function takes two arguments, \code{x},
#' a data.frame object, and \code{find_class}, a single or vector of character
#' value(s) of the class type desired.
#'
#' @param df data.frame; a \bold{data.frame} with named columns
#' @param find_class string or string vector; a single or vector of string
#'   value(s) of column class types
#' @examples
#' x <- data.frame(
#'   col_a = as.factor(c("a", "b", "c"))
#'   , col_b = as.integer(c(1, 2, 3))
#'   , col_c = as.factor(c("apple", "banana", "citrus"))
#'   , col_d = as.character(c("one", "two", "three"))
#' )
#' column_class(x, "factor")
#' column_class(x, c("integer", "character"))
#' @export
column_class <- function(df = NULL, find_class = NULL) {
  if (is.null(colnames(df))) {
    stop(
      stringr::str_c(
        "\nObject "
        , deparse(substitute(df))
        , " does not contain named columns.\n"
        )
      )
  } else if (is.null(find_class)) {
    #return all column names and classes
    c <- sapply(df, class)
  } else {
    # return all column names and classes where class matches
    c <- sapply(df, class)
    if (length(find_class) > 1) {
    c <- c[c %in% find_class]
    } else {
      c <- c[c == find_class]
    }
  }
  c
}
