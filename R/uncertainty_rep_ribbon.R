
uncertainty_rep_ribbon = function(..., n_sample = NA, draw = "collapse") {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var) {
    if (!is.na(n_sample) && ".draw" %in% colnames(samples)) {
      ndraw <- max(samples$.draw)
      sample_ids = sample(1:ndraw, n_sample)
      samples <- samples %>%
        dplyr::filter(.draw %in% sample_ids)
    }

    group_on = rlang::quo(.draw)

    if ("x_axis" %in% colnames(samples)) {
      if (is.function(draw)) {
        # if (is.null(agg_func)) {
        #   agg_func = mean
        # }

        if ("x_axis" %in% colnames(samples)) {
          agg_sample = samples %>%
            dplyr::group_by_at(c(ggplot2::vars(.draw), ggplot2::vars(x_axis), row_vars, col_vars)) %>%
            dplyr::summarise(y_agg = draw(!!y_var))
        } else {
          agg_sample = samples %>%
            dplyr::group_by_at(c(ggplot2::vars(.draw), row_vars, col_vars)) %>%
            dplyr::summarise(y_agg = draw(!!y_var))
        }
        p = ggdist::stat_ribbon(data = agg_sample,
                                ggplot2::aes(x = x_axis, y = y_agg,
                                             color = !!model_color,
                                             fill = !!model_color),
                                ...,
                                alpha = 0.5)
      } else if (draw == "collapse") {
        p = ggdist::stat_ribbon(data = samples,
                                    ggplot2::aes(x = x_axis, y = !!y_var,
                                                 color = !!model_color,
                                                 fill = !!model_color),
                                    ...,
                                    alpha = 0.5)
      } else if (draw == "group") {
        p = ggdist::stat_ribbon(data = samples,
                                    ggplot2::aes(x = x_axis, y = !!y_var, group = !!group_on,
                                                 color = !!model_color,
                                                 fill = !!model_color),
                                    ...,
                                    alpha = 0.5)
      } else if (draw == "hops") {
        hops_id = get_unique_id()
        draw_col = paste(".draw", hops_id, sep = "")
        p = c(ggdist::stat_ribbon(data = samples %>%
                                      dplyr::mutate(!!draw_col := !!group_on),
                                    ggplot2::aes(x = x_axis, y = !!y_var,
                                                 group = !!draw_col,
                                                 color = !!model_color,
                                                 fill = !!model_color),
                                    ...,
                                    alpha = 0.5),
              gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
      }
      return(p)
      # return(list(ggnewscale::new_scale_fill(), p, scale_fill,
      #             ggnewscale::new_scale_fill(), ggplot2::scale_fill_manual(name = "fill", values = colors_legend)))
    } else {
      warning("Conditional variable on x axis needs to be specified to draw ribbon geometry.")
      # if (".draw" %in% colnames(samples)) {
      #   x_seq = function(len) seq(min(samples[[rlang::quo_name(y_var)]]),
      #                             max(samples[[rlang::quo_name(y_var)]]), length.out = len)
      #   temp_samples <- samples %>%
      #     dplyr::group_by_at(c(ggplot2::vars(.draw), row_vars, col_vars)) %>%
      #     dplyr::filter(dplyr::n()> 1) %>%
      #     dplyr::summarise(x_ds = list(stats::density(!!y_var)$x),
      #                      y_ds = list(stats::density(!!y_var)$y)) %>%
      #     dplyr::rowwise() %>%
      #     dplyr::mutate(x_axis = list(x_seq(length(x_ds))),
      #                   y_axis = list(stats::approx(x_ds, y_ds, x_seq(base::length(x_ds)))$y)) %>%
      #     tidyr::unnest(c(x_axis, y_axis))
      #
      #   return(c(ggdist::stat_lineribbon(data = temp_samples,
      #                                    ggplot2::aes(x = x_axis, y = y_axis),
      #                                    ...,
      #                                    color = !!model_color,
      #                                    alpha = 0.5),
      #     ggplot2::scale_fill_brewer(palette = 7)))
      # } else {
      #   # TODO
      # }
    }
  }
}
