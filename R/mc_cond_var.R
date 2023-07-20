
#' Add marginal check to model check
#'
#' @param x A variable quoted by `ggplot2::vars()` and defining the conditional variable on x axis.
#' @param row,col A set of variables quoted by `ggplot2::vars()` and defining faceting groups on the rows or columns
#'
#' @export
#'
#' @examples
mc_condition_on = function(x = NULL, color = NULL, row = NULL, col = NULL) {
  p = function(mc_setting = NULL) {
    c(list(conditional_vars = list(x_var = x, color_var = color, row_vars = row, col_vars = col)), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
