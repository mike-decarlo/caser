#' Dataframe Column Sorting Function
#' \code{sort_columns} takes two arguments, the name of a dataframe and the order
#'   (ascending or descending), and will then provide the dataframe in the
#'   column sorting as specified.
#' @param x a dataframe object.
#' @param order asc or desc for ascending or descending ordering respectively.
#' @keywords sort columns dataframe
#' @export
sort_columns <- function(x = NULL, order = "asc") {
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
  message(
    paste0(
      "\nObject 'x' columns being sorted in "
      , ifelse(order == "asc", "ascending", "descending")
      , ", alphabetical order.\n"
    )
  )
  if (order == "asc") {
    x <- x[, order(colnames(x), decreasing = F)]
  } else if (order == "desc") {
    x <- x[, order(colnames(x), decreasing = T)]
  }
  return(x)
}
