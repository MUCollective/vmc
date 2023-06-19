
#' Add marginal check to model check
#'
#' @param x_var A variable quoted by `ggplot2::vars()` and defining the conditional variable on x axis.
#' @param row_vars,col_vars A set of variables quoted by vars() and defining faceting groups on the rows or columns dimension.
#'
#' @export
#'
#' @examples
mc_cond_var = function(x_var = NULL, row_vars = NULL, col_vars = NULL) {
  p = function(mc_setting = NULL) {
    c(list(conditional_vars = list(x_var = x_var, row_vars = row_vars, col_vars = col_vars)), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
