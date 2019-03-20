#' Dataframe Column Sorting Function
#' \code{sort_columns} takes two arguments, the name of a dataframe and the
#'   order (ascending or descending), and will then provide the dataframe in
#'   the column sorting as specified.
#' @param df data.frame; the source \bold{data.frame} to be sorted.
#' @param order string; if \code{"asc"}, the function sorts in
#'   ascending order; if \code{"desc"}, the function sorts in descending order.
#' @param verbose logical; if \code{TRUE}, the function prints messages about
#'   objects being sorted.
#' @keywords sort columns
#' @export
sort_columns <- function(df = NULL, order = "asc", verbose = FALSE) {
  if (is.null(df)) {
    stop(
      "The 'df' argument must be a non-null, dataframe object.\n"
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
        "\nObject 'df' columns being sorted in "
        , ifelse(order == "asc", "ascending", "descending")
        , ", alphabetical order.\n"
      )
    )
  }
  if (order == "asc") {
    df <- df[, order(colnames(df), decreasing = F)]
  } else if (order == "desc") {
    df <- df[, order(colnames(df), decreasing = T)]
  }
  df
}
