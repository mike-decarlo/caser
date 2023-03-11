#' @title Import a batch of files
#'
#' @description To import one or more files into a single df object. The files
#'   should all be housed in the same directory, the path to which is supplied
#'   as the sole argument.
#' @return An array with dimensions as sub-frame rows, sub-frame columns, and
#'  and sub-frame index.
#' @param .path string; the \bold{path} to the directory containing desired files.
#' @param .fun string; the \bold{function name} used for importing files.
#' @param .ext \code{(optional)} string; the file \bold{extension} to indicate
#'   type of external data file to be imported:\itemize{
#'   \item \code{".csv"}
#'   \item \code{".tsv"}
#'   \item \code{".xlsx"}
#'   }
#' @param ... \code{(optional)} additional arguments specific to \code{f}.
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
import_batch_files <- function(.path = NULL, .fun = NULL, .ext = NULL, ...) {
  # Check if .path is non-null and character class
  if (is.null(.path)) {
    stop("\nArgument '.path' must be non-null.\n")
  } else if (!is(.path, "character")) {
    stop(stringr::str_c(
      "\nArgument '.path' must be a character string.\n"
      )
    )
  }
  # Check if file exists
  if (!file.exists(.path)) {
    stop(stringr::str_c(
      "\nDirectory at '"
      , .path
      , "' does not exist.\n"
    ))
  }
  # Check that .fun is non-null
  if (is.null(.fun)) {
    stop("\nArgument '.fun' must be the non-null value of a function name.\n")
  }
  # If passes checks, start importing
  paths <- list.files(
    path = .path
    , all.files = FALSE
    , full.names = TRUE
    , pattern = .ext
  )
  d <- purrr::map(
    paths
    , .fun
    , ...
  )
  d <- purrr::map(
    d
    , caser::sort_columns
  )
  d <- dplyr::bind_rows(d)
}
