#' @title Random Number Generator
#'
#' @description \code{generate_random_number} draws a random number(s) from a
#'   range either as an integer or a float with a specified number of decimal
#'   digits
#' @param n numeric; a numeric value that specifies the number of random
#'   numbers the user would like returned
#' @param min numeric; a numeric value that specifies the minimum number in the
#'   range to draw from
#' @param max numeric; a numeric value that specifies the maximum number in the
#'   range to draw from
#' @param type string; a string that must be either \code{"integer"} or
#'   \code{"float"}, specifies the format of the desired, numeric output as
#'   either an \code{integer} or \code{float} value
#' @param digits \code{optional} numeric; a numeric value to accompany
#'   \code{type = "float"} and specify the number of decimal digits to include
#' @examples
#' generate_random_number(n = 1, min = 1, max = 10, type = "integer")
#'
#' generate_random_number(n = 2, min = 1, max = 10, type = "float", digits = 3)
#'
#' generate_random_number(
#'   n = 3, min = 1, max = 10, type = "float", digits = "5"
#'   )
#' @importFrom stats runif
#' @importFrom stringr str_c
#' @export
generate_random_number <- function(n = NULL, min = NULL, max = NULL
  , type = "integer", digits = NULL) {
  # Min should be less than or equal to max, if not switch values
  if (min > max) {
    message(
      stringr::str_c(
        "Warning: Min must be less than or equal to max.\n"
        , "Switching values to satisfy requirements.\n"
      )
    )
    temp <- min
    min <- max
    max <- temp
    rm(temp)
  }
  # Error handling for number of digits
  # Should be a number value (either as a numeric or character class)
  # Should be an integer
  if (type == "float" & is.null(digits)) {
    stop(
      "Error: Argument 'digits' must have a number value for type = 'float'.\n"
      )
  } else if (type == "float" & !is(digits, "numeric")) {
    if (is.na(as.numeric(digits))) {
      stop(
        stringr::str_c(
          "Error: Argument 'digits' must be a numeric or character string of a"
          , " number value.\nE.g., 1 or '1', but not 'e'.\n"
          )
        )
    } else {
      message(
        stringr::str_c(
          "Non-numeric value entered for argument 'digits'.\n"
          , "Converting to numeric...\n"
          )
        )
      digits <- round(as.numeric(digits), 0)
      (round(stats::runif(n = n, min = min, max = max), digits = digits))
    }
  } else if (type == "float") {
    (round(stats::runif(n = n, min = min, max = max), digits = digits))
  } else if (type == "integer") {
    digits <- 0
    (round(stats::runif(n = n, min = min, max = max), digits = digits))
  } else {
    stop(
      stringr::str_c(
        "Error: Argument 'type' must have a value of either 'integer' or"
        , "'float'.\n"
      )
    )
  }
}
