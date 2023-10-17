
#' Add marginal check to model check
#'
#' @param x A variable quoted by `ggplot2::vars()` and defining the conditional variable on x axis.
#' @param row,col A set of variables quoted by `ggplot2::vars()` and defining faceting groups on the rows or columns
#' @param color A variable quoted by `ggplot2::vars()` and defining the color aesthetic in model check.
#' @param scales The setting of scales in `ggplot2::facet_grid()`. Default to be `"fixed"`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' mcplot(mpg_model) +
#'   mc_condition_on(x = vars(disp))
#'
#' mcplot(mpg_model) +
#'   mc_condition_on(x = vars(disp), row = vars(vs), col = vars(am))
mc_condition_on = function(x = NULL, color = NULL, row = NULL, col = NULL, scales = "fixed") {
  p = function(mc_setting = NULL) {
    c(list(conditional_vars = list(x_var = x, color_var = color, row_vars = row, col_vars = col, scales = scales)), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
