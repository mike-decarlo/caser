#' @title Count Number of Decimal Digits Function
#'
#' @description \code{number_digits} takes one argument, an object x, that will
#'   be evaluated for the number of decimal digits it contains.
#' @param x scalar object.
#' @keywords digits decimal
#' @export
number_digits <- function(x) {
  if (is.na(suppressWarnings(as.numeric(x)))) {
    stop("Argument 'x' must be a non-strng, number.\n")
  } else if (!is(as.numeric(x), "numeric")) {
    warning("Argument 'x' should be a number in numeric format.\n")
    x <- as.numeric(x)
  }
  if ((x %% 1) != 0) {
    as.numeric(
      nchar(
        strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]]
      )
    )
  } else {
    0
  }
}
