#' Import a batch of files
#'
#' To import one or more files into a single df object. The files
#'   should all be housed in the same directory, the path to which is supplied
#'   as the sole argument.
#' @param p A string of the directory path containing desired files.
#' @param f A function name used for importing of individual files.
#' @return array with dimensions as sub-frame rows, sub-frame columns, and
#'  and sub-frame index.
#' @param ext Optional. A character string \itemize{
#'   \item \code{".csv"}
#'   \item \code{".tsv"}
#'   \item \code{".xlsx"}
#'   }
#' @importFrom stringr str_c
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
import_files_batch <- function(p = NULL, f = NULL, ext = NULL, ...) {
  # Check if p is non-null and character class
  if (is.null(p)) {
    stop("\nArgument 'p' must be non-null.\n")
  } else if (class(path) != "character") {
    warning(stringr::str_c(
      "\nArgument 'p' must be a character string.\n"
      , "Converting to class 'character'...\n"
    ))
    p <- as.character(p)
  }
  # Check if file exists
  if (!file.exists(p)) {
    stop(stringr::str_c(
      "\nDirectory at '"
      , p
      , "' does not exist.\n"
    ))
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
