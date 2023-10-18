
comp_layout_jux = function(p_obs, ...) {
  function(p_pred, samples, is_animation, color_var, row_vars, col_vars, scales, color,
           x_type, y_type, labels, gglayers, model_color, observed_color, ndraw) {
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

    if ("x_axis" %in% colnames(samples) && x_type != "quantitative" && !is.factor(samples$x_axis)) {
      samples = samples %>% dplyr::mutate(x_axis = factor(x_axis, levels = sort(unique(x_axis))))
    }


    if ("x_axis" %in% colnames(samples)) {
      # if (x_type == "quantitative" || is.factor(samples$x_axis)) {
        obs_p <- ggplot2::ggplot(mapping = ggplot2::aes(x = x_axis))
      # } else {
      #   obs_p <- ggplot2::ggplot(mapping = aes(x = factor(x_axis, levels = sort(unique(x_axis)))))
      # }
    } else {
      obs_p <- ggplot2::ggplot()
    }
    colors_legend = c("obs" = observed_color, "model" = model_color)
    if (is.null(color_var)) {
      obs_p <- obs_p + ggplot2::scale_color_manual(name = "color", values = colors_legend) +
        ggplot2::scale_fill_manual(name = "fill", values = colors_legend)
    }

    p_sup = p_pred
    for (obs_uncert_rep in p_obs) {
      obs = obs_uncert_rep(samples, row_vars, col_vars, labels, list(x_type = x_type, y_type = y_type),
                           color, is_animation, rlang::quo(observation))
      obs_p <- obs_p + obs
      p_sup <- p_sup + obs
    }

    if ("x_axis" %in% colnames(samples)) {
      obs_p <- obs_p + ggplot2::labs(x = labels$x, y = labels$y)
    } else {
      obs_p <- obs_p + ggplot2::labs(x = "density", y = labels$y)
    }

    p_obs <- obs_p +
      ggplot2::facet_grid(rows = row_vars, cols = col_vars, scales = scales) +
      gglayers

    x_lim = ggplot2::layer_scales(p_sup)$x$range$range
    y_lim = ggplot2::layer_scales(p_sup)$y$range$range

    p_obs = p_obs + ggplot2::lims(x = x_lim, y = y_lim)
    p_pred = p_pred + ggplot2::lims(x = x_lim, y = y_lim)

    if (is_animation) {
      temp = file.path(tempdir(), "obs_plot.png")
      ggsave(temp, p_obs, units = "px", dpi = 72, height = 400, width = 400)
      p_obs <- magick::image_read(temp)

      p_pred <- p_pred %>%
        gganimate::animate(renderer = gganimate::gifski_renderer(),
                           units = "px", res = 72, height = 400, width = 400) %>%
        magick::image_read()
      p <- magick::image_append(c(p_obs[1], p_pred[1]))
      for (i in 2:length(p_pred)) {
        combined <- magick::image_append(c(p_obs[1], p_pred[i]))
        p <- c(p, combined)
      }
      return(magick::image_animate(p, fps = 5, dispose = "previous"))
    } else {
      p = cowplot::plot_grid(p_obs, p_pred, ...)
      return(p)
    }
  }
}
