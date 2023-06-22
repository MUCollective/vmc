
comp_layout_exp = function(p_obs, ...) {
  function(p_pred, samples, is_animation, row_vars, col_vars, color,
           x_type, y_type, labels, gglayers, model_color, observed_color) {
    if (is_animation) {
      p = gganimate::animate(p_pred, renderer = gganimate::gifski_renderer(), fps = 5)
    } else {
      p = p_pred
    }
    p
  }
}
