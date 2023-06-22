
comp_layout_jux = function(p_obs, ...) {
  function(p_pred, samples, is_animation, row_vars, col_vars, color,
           x_type, y_type, labels, gglayers, model_color, observed_color) {
    if (!is_animation) {
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
    }


    if ("x_axis" %in% colnames(samples)) {
      if (x_type == "quantitative" || is.factor(samples$x_axis)) {
        obs_p <- ggplot2::ggplot(mapping = aes(x = x_axis))
      } else {
        obs_p <- ggplot2::ggplot(mapping = aes(x = factor(x_axis, levels = sort(unique(x_axis)))))
      }
    } else {
      obs_p <- ggplot2::ggplot()
    }
    colors_legend = c("obs" = observed_color, "model" = model_color)
    obs_p <- obs_p + ggplot2::scale_color_manual(name = "color", values = colors_legend) +
      ggplot2::scale_fill_manual(name = "fill", values = colors_legend)

    p_sup = p_pred
    for (obs_uncert_rep in p_obs) {
      obs = obs_uncert_rep(samples, row_vars, col_vars, labels, list(x_type = x_type, y_type = y_type),
                           color, is_animation, rlang::quo(observation), colors_legend)
      obs_p <- obs_p + obs
      p_sup <- p_sup + obs
    }

    if ("x_axis" %in% colnames(samples)) {
      obs_p <- obs_p + ggplot2::labs(x = labels$x, y = labels$y)
    } else {
      obs_p <- obs_p + ggplot2::labs(x = "density", y = labels$y)
    }

    p_obs <- obs_p +
      ggplot2::facet_grid(rows = row_vars, cols = col_vars, labeller = ggplot2::label_both) +
      gglayers

    x_lim = ggplot2::layer_scales(p_sup)$x$range$range
    y_lim = ggplot2::layer_scales(p_sup)$y$range$range

    p_obs = p_obs + ggplot2::lims(x = x_lim, y = y_lim)
    p_pred = p_pred + ggplot2::lims(x = x_lim, y = y_lim)

    if (is_animation) {
      p_obs <- p_obs +
        gganimate::transition_manual(.draw, cumulative = TRUE)

      p_obs <- p_obs %>%
        gganimate::animate(renderer = gganimate::gifski_renderer()) %>%
        magick::image_read()
      p_pred <- p_pred %>%
        gganimate::animate(renderer = gganimate::gifski_renderer()) %>%
        magick::image_read()
      p <- magick::image_append(c(p_obs[1], p_pred[1]))
      for (i in 2:length(p_pred)) {
        combined <- magick::image_append(c(p_obs[1], p_pred[i]))
        p <- c(p, combined)
      }
    } else {
      p = cowplot::plot_grid(p_obs, p_pred, ...)
    }
    p
  }
}
