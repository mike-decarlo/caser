#' @title Generates correlation plot with confidence bars
#'
#' @description The \code{plot_corr} function takes two arguments, \code{.data},
#' a data.frame object, and \code{var}, a variable string within the data.frame.
#'
#' @param .data data.frame; a \bold{data.frame} with named columns
#' @param var string; a single, quoted variable name
#' @examples
#' x <- data.frame(
#'   col_a = rnorm(100, 10, 2),
#'   col_b = rnorm(100, 25, 5),
#'   col_c = runif(100, 10, 25),
#'   col_d = seq(1, 200, 2)
#' )
#' plot_corr(.data, "col_a")
#' @importFrom dplyr select
#' @importFrom purrr map map_dfr
#' @importFrom broom tidy
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar labs
#' @export
plot_corr <- function(.data, var) {
  x <- dplyr::select(.data, -!!var)
  y <- unname(unlist(dplyr::select(.data, !!var)))
  res <- purrr::map(x, cor.test, y = y)
  
  (ggplot2::ggplot(
    data = purrr::map_dfr(res, broom::tidy, .id = "predictor"),
    ggplot2::aes(x = forcats::fct_reorder(predictor, estimate))) +
      ggplot2::geom_point(ggplot2::aes(y = estimate)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
      ggplot2::labs(x = NULL, y = paste("Correlation with", var)))
}
