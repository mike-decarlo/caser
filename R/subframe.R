#' A sub-framing function
#'
#' The \code{"subframe"} function is a subsetting function intended to
#'   produce equivalently size subsets from a single data frame. This is done
#'   by evaluating the number of observations in the original data set and
#'   calculating how many rows should be in each subset for a given nubmer of
#'   subsets.
#' The purpose behind \code{"subframe"} is to break apart large data frames into
#'   smaller ones for the sake of efficiency. This could be useful for test
#'   sets for code or method testing, etc.
#' @param df data frame with any number of columns or rows to be subsetted to
#'   equivalently sized sub-frames.
#' @param no_subs singel, numeric value of the number of sub-frames desired.
#'   Default is \code{NULL} in which case, if no value is entered, the frame
#'   will be divided into 10 sub-frames.
#' @return array with dimensions as sub-frame rows, sub-frame columns, and
#'  and sub-frame index.
#' @examples
#' # Create data frame
#' rand_frame <- data.frame(runif(100, 0, 1)
#'   , sample(c("M", "F", "Z"), 100, replace = TRUE)
#'   , rnorm(100, 0, 1))
#' colnames(rand_frame) <- c("runif", "sex_sample", "rnorm")
#'    
#' # Convert into sub-frame array (default "no_subs")
#' subbed_10 <- subframe(df = rand_frame)
#'  
#' # Convert into sub-frame array (5 "no_subs")
#' subbed_5 <- subframe(df = rand_frame, no_subs = 5)
#'
#' for (i in 1:dim(subbed_10)[3]) {
#'   assign(paste0("rand_frame_", i), subbed_10[, , i])
#' }
#' @export
subframe <- function(df, no_subs = NULL) {
  c_len <- ncol(df)
  r_len <- nrow(df)
  n_cell <- c_len * r_len
  if (is.null(no_subs)) {
    no_subs <- r_len / ((0.10 * n_cell) / c_len)
  }
  sub_rows <- r_len / no_subs
  for (i in 1:no_subs) {
    if (i == 1) {
      from_row <- 1
    } else {
      from_row <- from_row + sub_rows
    }
    to_row <- from_row + sub_rows - 1
    df_nm <- paste(deparse(substitute(df)), i, sep = "_")
    assign(df_nm, df[from_row:to_row,])
    if (i == 1) {
      for (m in 1:no_subs) {
        if (m == 1) {
          subnames <- c(paste0("sub_", m))
        } else {
        subnames <- c(subnames, paste0("sub_", m))
        }
      }
      subs <- array(dim = c(sub_rows, c_len, no_subs)
                    , dimnames = list(c(1:sub_rows)
                                      , c(colnames(df))
                                      , c(subnames)
                                      )
                    )
    }
    for (j in 1:sub_rows) {
      for (k in 1:c_len) {
        for (l in 1:no_subs) {
          subs[j, k, i] <- get(df_nm)[j, k]
        }
      }
    }
  }
  return(subs)
}
