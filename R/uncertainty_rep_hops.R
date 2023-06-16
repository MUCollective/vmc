
uncertainty_rep_hops = function(...) {
  function(samples, row_vars, col_vars, labels, axis_type, model_color) {
    zeallot::`%<-%`(c(x_type, y_type), axis_type)
    
    if ("x_axis" %in% colnames(samples)) {
      x_axis_order = sort(unique(samples$x_axis))
    }
    y_axis_order = sort(unique(samples$y_axis))
    
    is_animation = TRUE
    if ("x_axis" %in% colnames(samples)) {
      if (x_type == "quantitative" && y_type == "quantitative") {
        p = ggplot2::geom_point(data = samples, 
                                mapping = ggplot2::aes(x = x_axis, y = y_axis), 
                                ...,
                                color = model_color)
      } else if (y_type == "quantitative") {
        p = ggplot2::geom_point(data = samples |>
                                  dplyr::mutate(x_axis = factor(x_axis, levels = x_axis_order)), 
                                mapping = ggplot2::aes(x = x_axis, y = y_axis), 
                                ...,
                                color = model_color, 
                                shape = '-', 
                                size = 10)
      } else if (x_type == "quantitative") {
        p = ggplot2::geom_point(data = samples |>
                                  dplyr::mutate(y_axis = factor(y_axis, levels = y_axis_order)), 
                                mapping = ggplot2::aes(x = x_axis, y = y_axis), 
                                ...,
                                color = model_color, 
                                shape = '|',
                                size = 10)
      } else {
        p = c(ggplot2::geom_tile(data = samples |> 
                                 dplyr::mutate(x_axis = factor(x_axis, levels = x_axis_order),
                                               y_axis = factor(y_axis, levels = y_axis_order)) |>
                                 dplyr::group_by(x_axis, y_axis, .draw) |>
                                 dplyr::count(),
                               mapping = ggplot2::aes(x = x_axis, y = y_axis, fill = n),
                               ...),
              ggplot2::scale_fill_gradient2(name = 'Count of Records', low="white", high=model_color))
      }
    } else {
      if (y_type == "quantitative") {
        p = ggplot2::geom_density(data = samples, 
                                  mapping = ggplot2::aes(x = y_axis), 
                                  ...,
                                  color = model_color)
      } else {
        ndraws = max(samples$.draw)
        p = ggplot2::geom_bar(data = samples |> 
                                dplyr::mutate(y_axis = factor(y_axis, levels = y_axis_order)) |>
                                dplyr::count(y_axis, .draw) |> 
                                dplyr::group_by(.draw) |>
                                dplyr::mutate(sum_n = sum(n)),
                              mapping = ggplot2::aes(x = y_axis, y = n / sum_n),
                              stat = "identity",
                              ...,
                              color = model_color,
                              fill = model_color)
      }
    }
    c(p, gganimate::transition_manual(.draw, cumulative = FALSE))
  }
}