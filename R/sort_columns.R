#' @title Dataframe Column Sorting Function
#'
#' @description \code{sort_columns} takes two arguments, the name of a
#'   dataframe and the order (ascending or descending), and will then provide
#'   the dataframe in the column sorting as specified.
#' @param df data.frame; the source \bold{data.frame} to be sorted.
#' @param order string; if \code{"asc"}, the function sorts in
#'   ascending order; if \code{"desc"}, the function sorts in descending order.
#' @param verbose logical; if \code{TRUE}, the function prints messages about
#'   objects being sorted.
#' @keywords sort columns
#' @importFrom dplyr select
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
  if (!(
    stringr::str_to_lower(order) %in%
      c("asc", "a", "ascending", "desc", "d", "descending")
    )) {
    stop(
      stringr::str_c(
        "\nThe 'order' argument should have a value of 'asc/a/ascending' for ",
        "ascending or 'desc/d/descending' for descending ordering of the ",
        "dataframe columns.\n"
      )
    )
  }
  if (verbose == TRUE) {
    message(
      stringr::str_c(
        "\nObject 'df' columns being sorted in ",
        ifelse(
          stringr::str_to_lower(order) %in% c("asc", "a", "ascending"),
          "ascending",
          "descending"
        ),
        ", alphabetical order.\n"
      )
    )
  }
  if (stringr::str_to_lower(order) %in% c("asc", "a", "ascending")) {
    df <- dplyr::select(x, order(colnames(x), decreasing = FALSE))
  } else if (stringr::str_to_lower(order) %in% c("desc", "d", "descending")) {
    df <- dplyr::select(x, order(colnames(x), decreasing = TRUE))
  }
  df
}
