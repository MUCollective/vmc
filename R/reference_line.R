reference_line = function(..., n_sample = NA, draw = "collapse", group_on = NULL) {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var) {
    if (!is.na(n_sample) && ".draw" %in% colnames(samples)) {
      ndraw <- max(samples$.draw)
      sample_ids = sample(1:ndraw, n_sample)
      samples <- samples %>%
        dplyr::filter(.draw %in% sample_ids)
    }
    if (is.null(group_on)) {
      group_on = rlang::quo(.draw)
    }
    if (rlang::quo_name(group_on) == ".draw") {
      group_by_vars = ggplot2::vars(.row)
    } else {
      group_by_vars = ggplot2::vars(.draw)
    }
    if (is.function(draw)) {
      # if (is.null(agg_func)) {
      #   agg_func = ifelse(y_type == "quantitative", mean, function(v) {
      #     uniqv <- unique(v)
      #     uniqv[which.max(tabulate(match(v, uniqv)))]
      #   })
      # }
      if ("x_axis" %in% colnames(samples)) {
        agg_sample = samples %>%
          dplyr::group_by_at(c(group_by_vars, ggplot2::vars(x_axis), row_vars, col_vars)) %>%
          dplyr::summarise(y_agg = draw(!!y_var))
      } else {
        agg_sample = samples %>%
          dplyr::group_by_at(c(group_by_vars, row_vars, col_vars)) %>%
          dplyr::summarise(y_agg = draw(!!y_var))
      }

      p = ggplot2::geom_hline(data = agg_sample,
                              mapping = ggplot2::aes(yintercept = y_agg,
                                                     color = !!model_color),
                              ...)
    } else if (draw == "collapse") {
      p = ggplot2::geom_hline(data = samples,
                              mapping = ggplot2::aes(yintercept = !!y_var,
                                                     color = !!model_color),
                              ...)
    } else if (draw == "group") {
      p = ggplot2::geom_hline(data = samples,
                              mapping = ggplot2::aes(yintercept = !!y_var,
                                                     group = !!group_on,
                                                     color = !!model_color),
                              ...)
    } else if (draw == "hops") {
      hops_id = get_unique_id()
      draw_col = paste(".draw", hops_id, sep = "")
      p = c(ggplot2::geom_hline(data = samples %>%
                                  dplyr::mutate(!!draw_col := !!group_on),
                                mapping = ggplot2::aes(yintercept = !!y_var,
                                                       group = !!draw_col,
                                                       color = !!model_color),
                                ...),
            gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
    }
    p
  }
}
