
uncertainty_rep_pointinterval = function(..., n_sample = NA, draw = "collapse") {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var) {
    if (!is.na(n_sample) && ".draw" %in% colnames(samples)) {
      ndraw <- max(samples$.draw)
      sample_ids = sample(1:ndraw, n_sample)
      samples <- samples %>%
        dplyr::filter(.draw %in% sample_ids)
    }
    zeallot::`%<-%`(c(x_type, y_type), axis_type)

    if (draw == "collapse") {
      p = ggdist::stat_pointinterval(data = samples,
                                  ggplot2::aes(y = !!y_var,
                                               color = model_color),
                                  ...)
    } else if (draw == "group") {
      p = ggdist::stat_pointinterval(data = samples,
                                  ggplot2::aes(y = !!y_var, group = .draw,
                                               color = model_color),
                                  ...)
    } else if (draw == "hops") {
      hops_id = get_unique_id()
      draw_col = paste(".draw", hops_id, sep = "")
      p = c(ggdist::stat_pointinterval(data = samples %>%
                                      dplyr::mutate(!!draw_col := .draw),
                                    ggplot2::aes(y = !!y_var, group = .draw,
                                                 color = model_color),
                                    ...),
            gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
    } else if (is.function(draw)) {
      # if (is.null(agg_func)) {
      #   agg_func = mean
      # }

      p = ggdist::stat_pointinterval(data = samples %>%
                                    dplyr::group_by_at(c(ggplot2::vars(.row, x_axis), row_vars, col_vars)) %>%
                                    dplyr::summarise(y_agg = draw(!!y_var)),
                                  ggplot2::aes(y = y_agg,
                                               color = model_color),
                                  ...)
    }
    p
  }
}
