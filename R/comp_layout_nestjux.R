
comp_layout_nestjux = function(p_obs, ..., justification = .2) {
  function(p_pred, samples, is_animation, color_var, row_vars, col_vars, color,
           x_type, y_type, labels, gglayers, model_color, observed_color) {
    p <- p_pred
    if (".row" %in% colnames(samples)) {
      if ("x_axis" %in% colnames(samples)) {
        samples = samples %>%
          dplyr::group_by_at(c(ggplot2::vars(x_axis, observation, .row), color_var, row_vars, col_vars)) %>%
          dplyr::summarise()
      } else {
        samples = samples %>%
          dplyr::group_by_at(c(ggplot2::vars(observation, .row), color_var, row_vars, col_vars)) %>%
          dplyr::summarise()
      }
      samples = samples[!duplicated(samples), ]
    }

    if ("x_axis" %in% colnames(samples) && x_type != "quantitative") {
      for (layer in p$layers) {
        if (is.factor(samples$x_axis)) {
          layer$mapping$x = quo(as.numeric(x_axis) + 0.2)
        } else {
          layer$mapping$x = quo(as.numeric(factor(x_axis, levels = sort(unique(x_axis)))) + justification)
        }
      }
    }

    colors_legend = c("obs" = observed_color, "model" = model_color)

    for (obs_uncert_rep in p_obs) {
      obs = obs_uncert_rep(samples, row_vars, col_vars, labels, list(x_type = x_type, y_type = y_type),
                           color, is_animation, rlang::quo(observation))
      if ("x_axis" %in% colnames(samples) && x_type != "quantitative") {
        if (is.vector(obs) || is.list(obs)) {
          for (layer in obs) {
            if ("mapping" %in% names(layer)) {
              if (is.factor(samples$x_axis)) {
                layer$mapping$x = quo(as.numeric(x_axis) - 0.2)
              } else {
                layer$mapping$x = quo(as.numeric(factor(x_axis, levels = sort(unique(x_axis)))) - justification)
              }
            }
          }
        } else {
          if ("mapping" %in% names(obs)) {
            if (is.factor(samples$x_axis)) {
              obs$mapping$x = quo(as.numeric(x_axis) - 0.2)
            } else {
              obs$mapping$x = quo(as.numeric(factor(x_axis, levels = sort(unique(x_axis)))) - justification)
            }
          }
        }
      }
      p <- p + obs
    }

    if ("x_axis" %in% colnames(samples) && x_type != "quantitative") {
      if (is.factor(samples$x_axis)) {
        p <- p + ggplot2::scale_x_discrete(limits = levels(samples$x_axis))
      } else {
        p <- p + ggplot2::scale_x_discrete(limits = factor(sort(unique(samples$x_axis))))
      }
    }

    if (is_animation) {
      p = gganimate::animate(p, renderer = gganimate::gifski_renderer(), fps = 5)
    }
    p
  }
}
