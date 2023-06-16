
uncertainty_rep_static = function(...) {
  function(samples, row_vars, col_vars, labels, axis_type, model_color) {
    zeallot::`%<-%`(c(x_type, y_type), axis_type)
    
    if ("x_axis" %in% colnames(samples)) {
      if (x_type == "quantitative" && y_type == "quantitative") {
        p = ggplot2::geom_point(data = samples, 
                                mapping = ggplot2::aes(x = x_axis, y = y_axis), 
                                ...,
                                color = model_color)
      } else if (y_type == "quantitative") {
        p = ggplot2::geom_point(data = samples |>
                                  dplyr::mutate(x_axis = factor(x_axis)), 
                                mapping = ggplot2::aes(x = x_axis, y = y_axis), 
                                ...,
                                color = model_color, shape = '-', size = 10)
      } else if (x_type == "quantitative") {
        p = ggplot2::geom_point(data = samples |>
                                  dplyr::mutate(y_axis = factor(y_axis)), 
                                mapping = ggplot2::aes(x = x_axis, y = y_axis), 
                                ...,
                                color = model_color, shape = '|')
      } else {
        p = c(ggplot2::geom_tile(data = samples |> 
                                 dplyr::mutate(x_axis = factor(x_axis),
                                               y_axis = factor(y_axis)) |>
                                 dplyr::group_by(x_axis, y_axis) |>
                                 dplyr::count(),
                               mapping = ggplot2::aes(x = x_axis, y = y_axis, fill = n),
                               ...),
              ggplot2::scale_fill_gradient2(low="white", high=model_color))
      }
    } else {
      if (y_type == "quantitative") {
        p = ggplot2::geom_line(data = samples, 
                               mapping = ggplot2::aes(x = y_axis, group = .draw), stat="density", 
                               ...,
                               color = model_color, alpha = 0.2)
      } else {
        p = ggplot2::geom_bar(data = samples |> 
                                dplyr::mutate(y_axis = factor(y_axis)) |>
                                dplyr::count(y_axis),
                              mapping = ggplot2::aes(x = y_axis, y = n / sum(n)),
                              stat = "identity", 
                              ...,
                              color = model_color,
                              fill = model_color)
      }
    }
    p
  }
}