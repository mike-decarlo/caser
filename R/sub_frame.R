#' A sub-framing function
#'
#' The \code{"sub_frame"} function is a subsetting function intended to
#'   produce equivalently size subsets from a single data frame. This is done
#'   by evaluating the number of observations in the original data set and
#'   calculating how many rows should be in each subset for a given nubmer of
#'   subsets.
#' The purpose behind \code{"sub_frame"} is to break apart large data frames into
#'   smaller ones for the sake of efficiency. This could be useful for test
#'   sets for code or method testing, etc.
#' @param df data frame with any number of columns or rows to be subsetted to
#'   equivalently sized sub-frames.
#' @param num_subs single, numeric value of the number of sub-frames desired.
#'   Default is \code{NULL} in which case, if no value is entered, the frame
#'   will be divided into 10 sub-frames.
#' @return array with dimensions as sub-frame rows, sub-frame columns, and
#'  and sub-frame index.
#' @examples
#' # Data frame of 100 random observations from normal and uniform
#' df <- data.frame("X" = rnorm(100, 0, 1), "Y" = runif(100, 0, 1))
#' 
#' # Sub-frame to 2 data frames
#' sub_frame(df, 2)
#' 
#' # Sub-frame with default sub-framing
#' sub_frame(df)
#' @export
sub_frame <- function(df = NULL, num_subs = NULL) {
  if (is.null(df)) {
    stop("\nArgument 'df' must be a non-null object.\n")
  } else if (class(df) != "data.frame") {
    df <- as.data.frame(df)
    warning("\nArgument 'df' converted to data.frame class for sub-framing.\n")
  }
  if (is.null(num_subs)) {
    num_subs <- 10
  } else if (class(num_subs) != "numeric") {
    stop("\nArgument 'num_subs' must be numeric.\n")
  }
  options(warn = -1)
  split(df, rep(1:num_subs, each = ceiling(nrow(df) / num_subs), length.out = nrow(df)))
}
