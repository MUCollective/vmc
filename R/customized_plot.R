customized_plot = function(func, ...) {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var, colors_legend) {
    p = func(data = samples, 
             mapping = ggplot2::aes(x = x_axis, y = !!y_var), 
             ...,
             color = model_color)
    p
  }
}
