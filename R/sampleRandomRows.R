#' Randomly sample rows from a data frame
#' 
#' It is common to want a random sampling of data from a large data set for
#'   analysis of various forms. The \code{sampleRandomRows} function takes
#'   arguments needed to return a random sample of a data frame. Providing the
#'   source data frame, a sample size, and whether or not replacement is
#'   allowed in the sampling is all that is required for returning the sample.
#'   Row names (number typically) are returned in the sample making it easy to
#'   identify which observations are selected into the sample.
#' 
#' @param df source data frame to be sampled from
#' @param n sample size
#' @param replace shoould the smapling method use repalcement? \code{TRUE} if
#'   yes, \code{FALSE} (by default) if no
#'   
#' @keywords sample random row
#' 
#' @export
#' 
#' @examples 
#' iris_samp <- sampleRandomRows(iris, 5, replace = FALSE)

sampleRandomRows <- function(df, n, replace = FALSE) {
  
  if (replace == FALSE && n > nrow(df)) {
    
    message("The value for argument 'n' exceeds the number of rows in 'df'.\n")
    message("Please adjust 'n' to a value less than or equal to the number of 
            rows in 'df'. \n")
    stop()
      
  }
  
  return(data.frame(df[sample(x = nrow(df), size = n, replace = replace), ]))
  
}