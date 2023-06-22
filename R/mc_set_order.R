
#' Order ticks on axis, rows, and columns
#'
#' `mc_set_order` orders ticks on axis or grids on rows and columns. To make it ordered,
#'  the variables on axis, rows, and columns must be discrete.
#'
#' @param x,y,row,col A list of the order of x ticks, y ticks, rows, and columns.
#'
#' @export
#'
#' @examples
mc_set_order = function(x = NULL, y = NULL, row = NULL, col = NULL) {
  p = function(mc_setting = NULL) {
    c(list(order = list(x_order = x, y_order = y,
                        row_order = row, col_order = col)),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
