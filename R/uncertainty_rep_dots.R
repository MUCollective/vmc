
uncertainty_rep_dots = function(..., n_sample = NA, draw = "collapse") {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var) {
    if (!is.na(n_sample) && ".draw" %in% colnames(samples)) {
      ndraw <- max(samples$.draw)
      sample_ids = sample(1:ndraw, n_sample)
      samples <- samples %>%
        dplyr::filter(.draw %in% sample_ids)
    }
    zeallot::`%<-%`(c(x_type, y_type), axis_type)

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
      return(c(ggdist::stat_dots(data = agg_sample,
                                 ggplot2::aes(y = y_agg,
                                              color = !!model_color, fill = !!model_color),
                                 ...
      )))
    } else if (draw == "collapse") {
      return(c(ggdist::stat_dots(data = samples,
                                 ggplot2::aes(y = !!y_var,
                                              color = !!model_color, fill = !!model_color),
                                 ...
                                 )))
    } else if (draw == "group") {
      return(c(ggdist::stat_dots(data = samples,
                                 ggplot2::aes(y = !!y_var,
                                              group = !!group_on,
                                              color = !!model_color, fill = !!model_color),
                                 ...
                                 )))
    } else if (draw == "hops") {
      hops_id = get_unique_id()
      draw_col = paste(".draw", hops_id, sep = "")
      return(c(ggdist::stat_dots(data = samples %>%
                                   dplyr::mutate(!!draw_col := !!group_on),
                                 ggplot2::aes(y = !!y_var, group = !!draw_col,
                                              color = !!model_color, fill = !!model_color),
                                 ...
                                 ),
               gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE)))
    }
  }
}
