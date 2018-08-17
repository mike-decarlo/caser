#' Random Number Generator
#' 
#' \code{randnum} draws a random number from a range either as an integer or a float with a specified number of decimal digits
#' 
#' @param min a numeric argument, specifies the minimum number in the range to draw from
#' @param max a numeric argument, specifies the maximum number in the range to draw from
#' @param type a character argument, must be either \code{"integer"} or \code{"float"}, specifies the format of the desired, numeric output as either an \code{integer} or \code{float} value
#' @param digits \code{optional} a number value, to accompany \code{type = "float"} and specify the number of decimal digits to include
#' 
#' @examples
#' randnum(min = 1, max = 10, type = "integer")
#' 
#' randnum(min = 1, max = 10, type = "float", digits = 3)
#' 
#' randnum(min = 1, max = 10, type = "float", digits = "5")
#' 
#' @export
randnum <- function(min = 1, max = 1000, type = "integer", digits = NA) {
  # Error handling for number of digits
  # Should be a number value (either as a numeric or character class)
  # Should be an integer
  if(type == "float" & is.na(digits)) {
    message("Error: Argument 'digits' must have a number value for type = 'float'.\n")
    stop()
  } else if(type == "float" & class(digits) != "numeric") {
    if(is.na(as.numeric(digits))) {
      message("Error: Argument 'digits' must be a numeric or character string of a number value.\nE.g., 1 or '1', but not 'e'.\n")
      stop()
    } else {
      message("Non-numeric value entered for argument 'digits'.\n Converting to numeric...\n")
      digits <- round(as.numeric(digits), 0)
    }
  } else if(type == "float") {
    return(round(runif(1, min = min, max = max), digits = digits))
  } else if(type == "integer") {
    digits <- 0
    return(round(runif(1, min = min, max = max), digits = digits))
  } else {
    message("Error: Argument 'type' must have a value of either 'integer' or 'float'.\n")
  }
}
