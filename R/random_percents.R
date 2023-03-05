#' @title Return a vector of random proportions/percents that sum to 1/100
#'
#' @description Useful to split a data value into random parts that will sum
#'   back to the total.
#' @param .bin numeric; the number of \bold{bins} to generate.
#' @param .seed numeric; the \bold{seed} for randomly generating numbers.
#' @keywords random percent proportion
#' @examples
#' # With default seed value (1)
#' random_percents(.bin = 2)
#'
#' # With specific seed value (45)
#' random_percents(.bin = 2, .seed = 45)
#'
#' # With specific seed value (45) and more bins (9)
#' random_percents(.bin = 9, .seed = 45)
#'
#' @importFrom stats runif
#' @export
random_percents <- function(.bin = NULL, .seed = NULL) {
  if (!is.null(.seed)) {
    if (is.numeric(.seed)) {
      set.seed(.seed)
    } else {
      stop("\n'.seed' must be a numeric value.\n")
    }
  }
  if (is.null(.bin) || !is.numeric(.bin) || .bin == 0) {
    stop("\n'.bin' must be a numeric value > 0.\n")
  } else {
    x <- round(stats::runif(.bin, 0, 100), 0)
    (x / sum(x))
  }
}
