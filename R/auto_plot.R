auto_plot = function(..., n_sample = NA, draw = NULL) {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var, colors_legend) {
    if (!is.na(n_sample) && ".draw" %in% colnames(samples)) {
      ndraw <- max(samples$.draw)
      sample_ids = sample(1:ndraw, n_sample)
      samples <- samples %>%
        dplyr::filter(.draw %in% sample_ids)
    }
    zeallot::`%<-%`(c(x_type, y_type), axis_type)

    if ("x_axis" %in% colnames(samples)) {
      x_axis_order = sort(unique(samples$x_axis))
    }
    y_axis_order = sort(unique(samples[[rlang::quo_name(y_var)]]))

    StatDisProp = ggplot2::ggproto("StatDisProp", ggplot2::Stat,
                                   required_aes = c("x", "group"),
                                   compute_group = function(data, scales) {
                                     data %>%
                                       dplyr::count(x, group) %>%
                                       dplyr::group_by(group) %>%
                                       dplyr::mutate(prop = n / sum(n))
                                   })

    if ("x_axis" %in% colnames(samples)) {

      if (x_type == "quantitative" && y_type == "quantitative") {
        shape = 19
        size = 1.5
      } else if (y_type == "quantitative") {
        shape = '-'
        size = 10
      } else if (x_type == "quantitative") {
        shape = '|'
        size = 10
      }
      if (x_type == "quantitative" || y_type == "quantitative") {
        if (is.null(draw)) {
          draw = "collapse"
        }

        if (draw == "collapse") {
          p = ggplot2::geom_point(data = samples,
                                  mapping = ggplot2::aes(y = !!y_var,
                                                         color = model_color),
                                  ...,
                                  shape = shape,
                                  size = size)
        } else if (draw == "group") {
          p = ggplot2::geom_point(data = samples,
                                  mapping = ggplot2::aes(y = !!y_var,
                                                         group = .draw,
                                                         color = model_color),
                                  ...,
                                  shape = shape,
                                  size = size)
        } else if (draw == "hops") {
          hops_id = get_unique_id()
          draw_col = paste(".draw", hops_id, sep = "")
          p = c(ggplot2::geom_point(data = samples %>%
                                      dplyr::mutate(!!draw_col := .draw),
                                  mapping = ggplot2::aes(y = !!y_var,
                                                         color = model_color),
                                  ...,
                                  shape = shape,
                                  size = size),
                   gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
        } else if (is.function(draw)) {
          # if (is.null(agg_func)) {
          #   agg_func = ifelse(y_type == "quantitative", mean, function(v) {
          #     uniqv <- unique(v)
          #     uniqv[which.max(tabulate(match(v, uniqv)))]
          #   })
          # }

          p = ggplot2::geom_point(data = samples %>%
                                    dplyr::group_by_at(c(ggplot2::vars(.row, x_axis), row_vars, col_vars)) %>%
                                    dplyr::summarise(y_agg = draw(!!y_var)),
                                  mapping = ggplot2::aes(y = y_agg,
                                                         color = model_color),
                                  ...,
                                  shape = shape,
                                  size = size)
        }
      } else {
        if (is.null(draw)) {
          draw = "hops"
        }
        if (draw == "collapse") {
          p = c(ggplot2::geom_bin2d(data = samples,
                                    mapping = ggplot2::aes(y = factor(!!y_var, levels = y_axis_order),
                                                           fill = ggplot2::after_stat(count)),
                                    ...),
                ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color))
        } else if (draw == "group") {
          p = c(ggplot2::geom_bin2d(data = samples,
                                    mapping = ggplot2::aes(y = factor(!!y_var, levels = y_axis_order),
                                                           group = .draw,
                                                           fill = ggplot2::after_stat(count)),
                                    ...),
                ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color))
        } else if (draw == "hops") {
          hops_id = get_unique_id()
          draw_col = paste(".draw", hops_id, sep = "")
          p = c(ggplot2::geom_bin2d(data = samples %>%
                                      dplyr::mutate(!!draw_col := .draw),
                                    mapping = ggplot2::aes(y = factor(!!y_var, levels = y_axis_order),
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
                                      dplyr::group_by_at(c(ggplot2::vars(.row, x_axis), row_vars, col_vars)) %>%
                                      dplyr::summarise(y_agg = draw(y_axis)),
                                    mapping = ggplot2::aes(y = factor(y_agg, levels = y_axis_order),
                                                           fill = ggplot2::after_stat(count)),
                                    ...),
                ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color))
        }
      }
    } else {
      if (y_type == "quantitative") {
        if (is.null(draw)) {
          draw = "group"
        }
        if (draw == "collapse") {
          p = ggplot2::geom_line(data = samples,
                                 mapping = ggplot2::aes(x = !!y_var,
                                                        color = model_color), stat = "density",
                                 ...)
        } else if (draw == "group") {
          p = ggplot2::geom_line(data = samples,
                                 mapping = ggplot2::aes(x = !!y_var, group = .draw,
                                                        color = model_color), stat = "density",
                                 ..., alpha = .1)
        } else if (draw == "hops") {
          hops_id = get_unique_id()
          draw_col = paste(".draw", hops_id, sep = "")
          p = c(ggplot2::geom_line(data = samples %>%
                                     dplyr::mutate(!!draw_col := .draw),
                                 mapping = ggplot2::aes(x = !!y_var, group = !!rlang::sym(draw_col),
                                                        color = model_color), stat = "density",
                                 ...),
                gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
        } else if (is.function(draw)) {
          # if (is.null(agg_func)) {
          #   agg_func = mean
          # }
          p = ggplot2::geom_line(data = samples %>%
                                   dplyr::group_by_at(c(ggplot2::vars(.row), row_vars, col_vars)) %>%
                                   dplyr::summarise(y_agg = draw(!!y_var)),
                                 mapping = ggplot2::aes(x = y_agg,
                                                        color = model_color), stat = "density",
                                 ...)
        }
      } else {
        if (is.null(draw)) {
          draw = "collapse"
        }
        if (draw == "collapse") {
          p = ggplot2::geom_point(data = samples %>%
                                  dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)),
                                mapping = ggplot2::aes(x = y_axis, y = (..count..)/sum(..count..),
                                                       color = model_color,
                                                       fill = model_color),
                                stat = "count",
                                ...,
                                shape = "-",
                                size = 10)
        } else if (draw == "group") {
          p = ggplot2::geom_point(data = samples %>%
                                    dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)),
                                  mapping = ggplot2::aes(x = y_axis,
                                                         y = ..prop..,
                                                         group = .draw,
                                                         color = model_color,
                                                         fill = model_color),
                                  stat = StatDisProp,
                                  ...,
                                  shape = "-",
                                  size = 10)
        } else if (draw == "hops") {
          hops_id = get_unique_id()
          draw_col = paste(".draw", hops_id, sep = "")
          p = c(ggplot2::geom_point(data = samples %>%
                                    dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)) %>%
                                    dplyr::mutate(!!draw_col := .draw),
                                  mapping = ggplot2::aes(x = y_axis, y = (..count..)/sum(..count..),
                                                         group = !!rlang::sym(draw_col),
                                                         color = model_color,
                                                         fill = model_color),
                                  stat = "count",
                                  ...,
                                  shape = "-",
                                  size = 10),
                gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE))
        } else if (is.function(draw)) {
          # if (is.null(agg_func)) {
          #   agg_func = function(v) {
          #     uniqv <- unique(v)
          #     uniqv[which.max(tabulate(match(v, uniqv)))]
          #   }
          # }
          p = ggplot2::geom_point(data = samples %>%
                                    dplyr::mutate(y_axis = factor(!!y_var, levels = y_axis_order)) %>%
                                    dplyr::group_by_at(c(ggplot2::vars(.row), row_vars, col_vars)) %>%
                                    dplyr::summarise(y_agg = draw(!!y_var)),
                                  mapping = ggplot2::aes(x = y_agg, y = (..count..)/sum(..count..),
                                                         color = model_color,
                                                         fill = model_color),
                                  stat = "count",
                                  ...,
                                  shape = "-",
                                  size = 10)
        }
      }
    }
    p
  }
}
