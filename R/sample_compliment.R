#' @title A Sampling Compliment Function
#'
#' @description Designed to work in tandem with \code{sample_rows} to return
#'   a complimentary data set to a sample from an original set.
#' @param orig an object; the original/complete data set being sampled
#' @param samp an object; a subset, randomly sampled from the \code{orig}
#' @examples
#' iris_test <- sample_rows(
#'   x = iris
#'   , n = floor(0.2 * nrow(iris))
#'   , seed = 1
#'   , replace = FALSE
#' )
#' iris_train <- sample_compliment(orig = iris, samp = iris_test)
#' @export
sample_compliment <- function(orig = NULL, samp = NULL) {
  if (is.null(orig)) {
    stop("Argument 'orig' must be a non-NULL data.frame.\n")
  } else {
    orig <- as.data.frame(orig)
  }
  if (is.null(samp)) {
    stop("Argument 'samp' must be a non-NULL data.frame.\n")
  } else {
    samp <- as.data.frame(samp)
  }
  rn <- rownames(orig)
  inc <- rownames(orig) %in% rownames(samp)
  rn <- rn[inc == FALSE]
  c <- as.data.frame(orig[-as.numeric(rownames(samp)), ])
  colnames(c) <- colnames(orig)
  rownames(c) <- rn
  c
}
