#' @title Generate a Data Frame with Basic Experimental Data
#'
#' @description \code{generate_experiment} takes two arguments, n is the number
#'   of experimental observations, and threshold is the success/failure
#'   threshold level
#' @param n a numeric, integer value indicating the number of observations in
#'   the experiment
#' @param threshold a numeric, float value indicating the threshold cutoff
#'   where a value less than or equal to the threshold is considered a success
#'   (TRUE) and a value greater than the cutoff is a failure (FALSE)
#' @keywords experiment
#' @export
generate_experiment <- function(n = 1000, threshold = 0.01) {
  digi <- caser::number_digits(threshold)
  nums <- caser::generate_random_number(
    n = n,
    min = 0,
    max = 1,
    type = "float",
    digits = digi
  )
  bools <- ifelse(nums <= threshold, TRUE, FALSE)
  data.frame(num_vals = nums, bool_vals = bools)
}
