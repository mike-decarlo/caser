#' @title Generate a Data Frame with Basic Experimental Data
#'
#' @description \code{generate_experiment} takes two arguments, n is the number
#'   of experimental observations, and threshold is the success/failure
#'   threshold level
#' @param n a numeric, integer value.
#' @param x a numeric, float value.
#' @keywords experiment
#' @export
generate_experiment <- function(n = 1000, threshold = 0.01) {
  digi <- number_digits(threshold)
  nums <- generate_random_number(
    n = n,
    min = 0,
    max = 1,
    type = "float",
    digits = digi
  )
  
  bools <- ifelse(nums <= threshold, TRUE, FALSE)
  
  data.frame(num_vals = nums, bool_vals = bools)
}
