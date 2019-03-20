#' Sample Random Rows from Data Frame
#'
#' It is common to want a random sampling of data from a large data set for
#'   analysis of various forms. The \code{sample_rows} function takes
#'   arguments needed to return a random sample of a data frame. Providing the
#'   source data frame, a sample size, and whether or not replacement is
#'   allowed in the sampling is all that is required for returning the sample.
#'   Row names (number typically) are returned in the sample making it easy to
#'   identify which observations are selected into the sample.
#' @param df data.frame; the source \bold{data.frame} to be sampled from
#' @param n numeric; sample size
#' @param s \code{(optional)} numeric; the \bold{seed} for randomly generating
#'   numbers.
#' @param replace logical; should the sampling method use repalcement?
#'   \code{TRUE} if yes, \code{FALSE} (by default) if no
#' @keywords sample random row
#' @examples
#' iris_samp <- sample_rows(iris, 5, replace = FALSE)
#' @export
sample_rows <- function(df = NULL, n, s = NULL, replace = FALSE) {
  if (is.null(df) || class(df) != "data.frame") {
    stop("\nThe argument 'df' must be a non-NULL data.frame.\n")
  }
  if (replace == FALSE && n > nrow(df)) {
    message(
      paste0(
        "The value for argument 'n' exceeds the number of rows in 'df'.\n"
        , "Please adjust 'n' to a value less than or equal to the number of"
        , " rows in 'df'. \n"
        )
      )
    stop()
  }
  if (!is.null(s)){
    set.seed(s)
  }
  d <- as.data.frame(df[sample(x = nrow(df), size = n, replace = replace), ])
  colnames(d) <- colnames(df)
  d
}
