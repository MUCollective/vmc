
#' Order ticks on axis, rows, and columns
#'
#' `mc_set_order` orders ticks on axis or grids on rows and columns. To make it ordered,
#'  the variables on axis, rows, and columns must be discrete.
#'
#' @param x_order,y_order,row_order,col_order A list of the order of x ticks, y ticks, rows, and columns.
#'
#' @export
#'
#' @examples
mc_set_order = function(x_order = NULL, y_order = NULL, row_order = NULL, col_order = NULL) {
  p = function(mc_setting = NULL) {
    c(list(order = list(x_order = x_order, y_order = y_order,
                        row_order = row_order, col_order = col_order)),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
