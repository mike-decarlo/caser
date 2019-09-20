#' @title A Sampling Compliment Function
#'
#' @description Designed to work in tandem with \code{sample_rows} to return
#'   a complimentary data set to a sample from an original set.
#' @param orig a data.frame; the original/complete data set being sampled
#' @param samp a data.frame; a subset, randomly sampled from the \code{orig}
#' @examples
#' iris_test <- sample_rows(
#'   df = iris
#'   , n = floor(0.2 * nrow(iris))
#'   , s = 1
#'   , replace = FALSE
#' )
#' iris_train <- sample_compliment(orig = iris, samp = iris_test)
#' @export
sample_compliment <- function(orig, samp) {
  c <- as.data.frame(orig[-as.numeric(rownames(samp)), ])
}
