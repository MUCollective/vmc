
uncertainty_rep_interval = function(..., n_sample = NA, draw = "collapse") {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var) {
    if (!is.na(n_sample) && ".draw" %in% colnames(samples)) {
      ndraw <- max(samples$.draw)
      sample_ids = sample(1:ndraw, n_sample)
      samples <- samples %>%
        dplyr::filter(.draw %in% sample_ids)
    }

    group_on = rlang::quo(.draw)

    y_axis_order = sort(unique(samples[[rlang::quo_name(y_var)]]))

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
      p = ggdist::stat_interval(data = agg_sample,
                                ggplot2::aes(y = y_agg,
                                             color = !!model_color),
                                ...,
                                alpha = .5)
    } else if (draw == "collapse") {
      p = ggdist::stat_interval(data = samples,
                                             ggplot2::aes(y = !!y_var,
                                                          color = !!model_color),
                                             ...,
                                alpha = .5)
    } else if (draw == "group") {
      p = ggdist::stat_interval(data = samples,
                                             ggplot2::aes(y = !!y_var,
                                                          group = !!group_on,
                                                          color = !!model_color),
                                             ...,
                                alpha = .5)
    } else if (draw == "hops") {
      hops_id = get_unique_id()
      draw_col = paste(".draw", hops_id, sep = "")
      p = c(ggdist::stat_interval(data = samples %>%
                                               dplyr::mutate(!!draw_col := !!group_on),
                                             ggplot2::aes(y = !!y_var, group = !!draw_col,
                                                          color = !!model_color),
                                             ...,
                                  alpha = .5),
               gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
    }

    return(p)

    # return(list(ggnewscale::new_scale_color(), p, scale_fill,
    #             ggnewscale::new_scale_color(), ggplot2::scale_color_manual(name = "color", values = colors_legend)))
  }
}
