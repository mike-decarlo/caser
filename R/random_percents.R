#' @title Return a vector of random proportions/percents that sum to 1/100
#'
#' @description Useful to split a data value into random parts that will sum back to the
#'   total.
#' @param b numeric; the number of \bold{bins} to generate.
#' @param s numeric; the \bold{seed} for randomly generating numbers.
#' @keywords random percent proportion
#' @examples
#' # With default seed value (1)
#' random_percents(b = 2)
#'
#' # With specific seed value (45)
#' random_percents(b = 2, s = 45)
#'
#' # With specific seed value (45) and more bins (9)
#' random_percents(b = 9, s = 45)
#'
#' @export
random_percents <- function(b = NULL, s = NULL) {
  if (!is.null(s)){
    if (is.numeric(s)){
      set.seed(s)
    } else {
      stop("\n's' must be a numeric value.\n")
    }
  }
  if (is.null(b) || !is.numeric(b) || b == 0) {
    stop("\n'b' must be a numeric value > 0.\n")
  } else {
    x <- round(runif(b, 0, 100), 0)
    (x / sum(x))
  }
}
