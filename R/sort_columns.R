#' Dataframe Column Sorting Function
#' \code{sort_columns} takes two arguments, the name of a dataframe and the
#'   order (ascending or descending), and will then provide the dataframe in
#'   the column sorting as specified.
#' @param x dataframe object
#' @param order character string; if \code{"asc"}, the function sorts in
#'   ascending order; if \code{"desc"}, the function sorts in descending order.
#' @param verbose logical; if \code{TRUE}, the function prints messages about
#'   objects being sorted.
#' @keywords sort columns
#' @export
sort_columns <- function(x = NULL, order = "asc", verbose = FALSE) {
  if (is.null(x)) {
    stop(
      "The 'x' argument must be a non-null, dataframe object.\n"
    )
  }
  if (class(order) != "character") {
    stop(
      "\nThe 'order' argument requires a character input of 'asc' or 'desc'.\n"
    )
  }
  if (!(order %in% c("asc", "desc"))) {
    stop(
      paste0(
        "\nThe 'order' argument should have a value of 'asc' for ascending or "
        , "'desc' for descending ordering of the dataframe columns.\n"
      )
    )
  }
  if (verbose == TRUE) {
    message(
      paste0(
        "\nObject 'x' columns being sorted in "
        , ifelse(order == "asc", "ascending", "descending")
        , ", alphabetical order.\n"
      )
    )
  }
  if (order == "asc") {
    x <- x[, order(colnames(x), decreasing = F)]
  } else if (order == "desc") {
    x <- x[, order(colnames(x), decreasing = T)]
  }
  x
}
