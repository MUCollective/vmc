
comp_layout_sup = function(p_obs, ...) {
  function(p_pred, samples, is_animation, row_vars, col_vars, color, 
           x_axis_order, y_axis_order, x_type, y_type, labels, gglayers, model_color, observed_color) {
    p <- p_pred
    
    if ("x_axis" %in% colnames(samples)) {
      samples = samples %>% 
        dplyr::group_by_at(c(ggplot2::vars(x_axis, observation, .row), row_vars, col_vars)) %>%
        dplyr::summarise()
    } else {
      samples = samples %>% 
        dplyr::group_by_at(c(ggplot2::vars(observation, .row), row_vars, col_vars)) %>%
        dplyr::summarise()
    }
    samples = samples[!duplicated(samples), ]
    
    for (obs_uncert_rep in p_obs) {
      obs = obs_uncert_rep(samples, row_vars, col_vars, labels, list(x_type = x_type, y_type = y_type), 
                           color, is_animation, rlang::quo(observation))
      p <- p + obs
    }
    if (is_animation) {
      p = gganimate::animate(p, renderer = gganimate::gifski_renderer(), fps = 5)
    }
    p
  }
}
