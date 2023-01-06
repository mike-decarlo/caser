#' @title Count Number of Decimal Digits Function
#'
#' @description \code{number_digits} takes one argument, an object x, that will
#'   be evaluated for the number of decimal digits it contains.
#' @param x scalar object.
#' @keywords digits decimal
#' @export
number_digits <- function(x) {
  x <- tryCatch(
    as.numeric(x),
    warning = function(w) {
      stop("Argument 'x' must be a number.")
    }
  )
  
  if ((x %% 1) != 0) {
    as.numeric(
      nchar(
        strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]]
      )
    )
  } else {
    0
  }
}
