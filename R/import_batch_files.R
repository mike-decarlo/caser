#' Import a batch of files
#'
#' To import one or more files into a single df object. The files
#'   should all be housed in the same directory, the path to which is supplied
#'   as the sole argument.
#' @return An array with dimensions as sub-frame rows, sub-frame columns, and
#'  and sub-frame index.
#' @param p string; the \bold{path} to the directory containing desired files.
#' @param f string; the \bold{function name} used for importing files.
#' @param ext \code{(optional)} string; the file \bold{extension} to indicate
#'   type of external data file to be imported:\itemize{
#'   \item \code{".csv"}
#'   \item \code{".tsv"}
#'   \item \code{".xlsx"}
#'   }
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
import_batch_files <- function(p = NULL, f = NULL, ext = NULL, ...) {
  # Check if p is non-null and character class
  if (is.null(p)) {
    stop("\nArgument 'p' must be non-null.\n")
  } else if (class(p) != "character") {
    stop(stringr::str_c(
        "\nArgument 'p' must be a character string.\n"
      )
    )
  }
  # Check if file exists
  if (!file.exists(p)) {
    stop(stringr::str_c(
      "\nDirectory at '"
      , p
      , "' does not exist.\n"
    ))
  }
  # Check that f is non-null
  if (is.null(f)) {
    stop("\nArgument 'f' must be the non-null value of a function name.\n")
  }
  # If passes checks, start importing
  paths <- list.files(
    path = p
    , all.files = F
    , full.names = T
    , pattern = ext
  )
  d <- purrr::map(
    paths
    , f
    , ...
  )
  d <- purrr::map(
    d
    , caser::sort_columns
  )
  d <- dplyr::bind_rows(d)
}
