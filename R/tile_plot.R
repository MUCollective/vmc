tile_plot = function(..., n_sample = NA, draw = "hops") {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var, colors_legend) {
    if (!is.na(n_sample) && ".draw" %in% colnames(samples)) {
      ndraw <- max(samples$.draw)
      sample_ids = sample(1:ndraw, n_sample)
      samples <- samples %>%
        dplyr::filter(.draw %in% sample_ids)
    }
    zeallot::`%<-%`(c(x_type, y_type), axis_type)

    y_axis_order = sort(unique(samples[[rlang::quo_name(y_var)]]))

    if (draw == "collapse") {
      p = c(ggplot2::geom_bin2d(data = samples %>%
                                  dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)),
                                mapping = ggplot2::aes(x = x_axis, y = y_axis,
                                                       fill = ggplot2::after_stat(count)),
                                ...),
            ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color))
    } else if (draw == "group") {
      p = c(ggplot2::geom_bin2d(data = samples %>%
                                  dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)),
                                mapping = ggplot2::aes(x = x_axis, y = y_axis, group = .draw,
                                                       fill = ggplot2::after_stat(count)),
                                ...),
            ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color))
    } else if (draw == "hops") {
      hops_id = get_unique_id()
      draw_col = paste(".draw", hops_id, sep = "")
      p = c(ggplot2::geom_bin2d(data = samples %>%
                                  dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)) %>%
                                  dplyr::mutate(!!draw_col := .draw),
                                mapping = ggplot2::aes(x = x_axis, y = y_axis,
                                                       fill = ggplot2::after_stat(count)),
                                ...),
            ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color),
            gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
    } else if (is.function(draw)) {
      # if (is.null(agg_func)) {
      #   agg_func = function(v) {
      #     uniqv <- unique(v)
      #     uniqv[which.max(tabulate(match(v, uniqv)))]
      #   }
      # }
      p = c(ggplot2::geom_bin2d(data = samples %>%
                                  dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)) %>%
                                  dplyr::group_by_at(c(ggplot2::vars(.row, x_axis), row_vars, col_vars)) %>%
                                  dplyr::summarise(y_agg = draw(y_axis)),
                                mapping = ggplot2::aes(x = x_axis, y = y_agg,
                                                       fill = ggplot2::after_stat(count)),
                                ...),
            ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color))
    }
    p
  }
}
