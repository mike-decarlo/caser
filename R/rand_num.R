#' Random Number Generator
#'
#' \code{rand_num} draws a random number(s) from a range either as an integer or a
#'   float with a specified number of decimal digits
#' @param n a numeric argument, specifies the number of random numbers the user
#'   would like returned
#' @param min a numeric argument, specifies the minimum number in the range to
#'   draw from
#' @param max a numeric argument, specifies the maximum number in the range to
#'   draw from
#' @param type a character argument, must be either \code{"integer"} or
#'   \code{"float"}, specifies the format of the desired, numeric output as
#'   either an \code{integer} or \code{float} value
#' @param digits \code{optional} a number value, to accompany
#'   \code{type = "float"} and specify the number of decimal digits to include
#' @examples
#' rand_num(min = 1, max = 10, type = "integer")
#'
#' rand_num(min = 1, max = 10, type = "float", digits = 3)
#'
#' rand_num(min = 1, max = 10, type = "float", digits = "5")
#' @export
rand_num <- function(n = NULL, min = NULL, max = NULL, type = NULL, digits = NULL) {
  # Min should be less than or equal to max, if not switch values
  if (min > max) {
    message(
      paste0(
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
  } else if (type == "float" & class(digits) != "numeric") {
    if (is.na(as.numeric(digits))) {
      stop(
        paste0(
          "Error: Argument 'digits' must be a numeric or character string of a"
          , " number value.\nE.g., 1 or '1', but not 'e'.\n"
          )
        )
    } else {
      message(
        paste0(
          "Non-numeric value entered for argument 'digits'.\n"
          , "Converting to numeric...\n"
          )
        )
      digits <- round(as.numeric(digits), 0)
    }
  } else if (type == "float") {
    return(round(runif(n = n, min = min, max = max), digits = digits))
  } else if (type == "integer") {
    digits <- 0
    return(round(runif(n = n, min = min, max = max), digits = digits))
  } else {
    stop(
      paste0(
        "Error: Argument 'type' must have a value of either 'integer' or"
        , "'float'.\n"
        )
    )
  }
}
