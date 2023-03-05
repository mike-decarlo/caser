#' @title A sub-framing function
#'
#' @description The \code{"subset_frame"} function is a subsetting function
#'   intended to produce equivalently size subsets from a single data frame.
#'   This is done by evaluating the number of observations in the original data
#'   set and calculating how many rows should be in each subset for a given
#'   number of subsets.
#' The purpose behind \code{"subset_frame"} is to break apart large data frames
#'   into smaller ones for the sake of efficiency. This could be useful for
#'   test sets for code or method testing, etc.
#' @param .data data.frame; a \bold{data.frame} with any number of columns or rows
#'   to be sub-setted to equally sized sub-frames.
#' @param n numeric; value of the \bold{number of sub-frames} desired. Defaults
#'   to \code{NULL} in which case, if no value is entered, the frame will be
#'   divided into 10 sub-frames.
#' @return array with dimensions as sub-frame rows, sub-frame columns, and
#'  and sub-frame index.
#' @examples
#' # Data.frame of 100 random observations from normal and uniform
#' df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))
#'
#' # Sub-frame to 2 data frames
#' subset_frame(df, 2)
#'
#' # Sub-frame with default sub-framing
#' subset_frame(df)
#' @export
subset_frame <- function(.data = NULL, n = NULL) {
  if (is.null(.data)) {
    stop("\nArgument '.data' must be a non-null object.\n")
  } else if (!is(.data, "data.frame")) {
    .data <- as.data.frame(.data)
    message("\nArgument '.data' converted to data.frame class for sub-framing.\n")
  }
  if (is.null(n)) {
    n <- 10
  } else if (!is(n, "numeric")) {
    stop("\nArgument 'num_subs' must be numeric.\n")
  }
  options(warn = -1)
  split(
    .data
    , rep(
      1:n
      , each = ceiling(nrow(.data) / n)
      , length.out = nrow(.data)
    )
    )
}
