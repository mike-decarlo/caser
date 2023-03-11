#' @title Sample Random Rows from Data Frame
#'
#' @description It is common to want a random sampling of data from a large
#'   data set for analysis of various forms. The \code{sample_rows} function
#'   takes arguments needed to return a random sample of a data frame.
#'   Providing the source data frame, a sample size, and whether or not
#'   replacement is allowed in the sampling is all that is required for
#'   returning the sample. Row names (number typically) are returned in the
#'   sample making it easy to identify which observations are selected into the
#'   sample.
#' @param .data an object; the source \bold{data.frame} to be sampled from
#' @param n a numeric value; sample size
#' @param .seed \code{(optional)} a numeric value; the \bold{seed} for randomly
#'   generating numbers.
#' @param replace a logical value; should the sampling method use replacement?
#'   \code{TRUE} if yes, \code{FALSE} (by default) if no
#' @keywords sample random row
#' @examples
#' iris_samp <- sample_rows(iris, 5, replace = FALSE)
#' @export
sample_rows <- function(.data = NULL, n, .seed = 1, replace = FALSE) {
  .data <- as.data.frame(.data)
  rownames(.data) <- seq_len(nrow(.data))
  if (replace == FALSE && n > nrow(.data)) {
    stop(
      stringr::str_c(
        "The value for argument 'n' exceeds the number of rows in 'df'.\n",
        "Please adjust 'n' to a value less than or equal to the number of",
        " rows in 'df'. \n"
      )
      )
  }
  set.seed(.seed)
  r <- sample(x = nrow(.data), size = n, replace = replace)
  d <- as.data.frame(.data[r, ])
  colnames(d) <- colnames(.data)
  rownames(d) <- r
  d
}
