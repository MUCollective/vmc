
mc_set_order = function(x = NULL, y = NULL, row = NULL, col = NULL) {
  p = function(mc_setting = NULL) {
    c(list(order = list(x_order = x, y_order = y,
                        row_order = row, col_order = col)),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
