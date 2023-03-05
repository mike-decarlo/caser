#' @title Data.frame Column Sorting Function
#'
#' @description \code{sort_columns} takes two arguments, the name of a
#'   data.frame and the order (ascending or descending), and will then provide
#'   the data.frame in the column sorting as specified.
#' @param .data data.frame; the source \bold{data.frame} to be sorted.
#' @param order string; if \code{"asc"}, the function sorts in
#'   ascending order; if \code{"desc"}, the function sorts in descending order.
#' @param verbose logical; if \code{TRUE}, the function prints messages about
#'   objects being sorted.
#' @keywords sort columns
#' @importFrom dplyr select
#' @export
sort_columns <- function(.data = NULL, order = "asc", verbose = FALSE) {
  if (is.null(.data)) {
    stop(
      "The '.data' argument must be a non-null, dataframe object.\n"
    )
  }
  if (!is(order, "character")) {
    stop(
      "\nThe 'order' argument requires a character input of 'asc/a/ascending'",
      " or 'desc/d/descending'.\n"
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
        "\nObject '.data' columns being sorted in ",
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
    .data <- dplyr::select(.data, order(colnames(.data), decreasing = FALSE))
  } else if (stringr::str_to_lower(order) %in% c("desc", "d", "descending")) {
    .data <- dplyr::select(.data, order(colnames(.data), decreasing = TRUE))
  }
  .data
}
