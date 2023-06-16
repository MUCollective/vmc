

obs_plot = function(...) {
  function(samples, is_animation, row_vars, col_vars, observed_color, 
           x_axis_order, y_axis_order, x_type, y_type, labels, x_lim, y_lim, is_meta = FALSE) {
    if (is_meta) {
      if ("x_axis" %in% colnames(samples)) {
        samples = samples |> 
          dplyr::group_by_at(c(ggplot2::vars(x_axis, observation, .row), row_vars, col_vars)) |>
          dplyr::summarise()
      } else {
        samples = samples |> 
          dplyr::group_by_at(c(ggplot2::vars(observation, .row), row_vars, col_vars)) |>
          dplyr::summarise()
      }
      samples = samples[!duplicated(samples), ]
    }
    
    if ("x_axis" %in% colnames(samples)) {
      if (x_type == "quantitative" && y_type == "quantitative") {
        p_obs = list(ggplot2::geom_point(data = samples, 
                                    mapping = ggplot2::aes(x = x_axis, y = observation), 
                                    ...,
                                    color = observed_color))
      } else if (y_type == "quantitative") {
        p_obs = list(ggplot2::geom_point(data = samples |>
                                      dplyr::mutate(x_axis = factor(x_axis)), 
                                    mapping = ggplot2::aes(x = x_axis, y = observation), 
                                    ...,
                                    color = observed_color, shape = "-", size = 10))
      } else if (x_type == "quantitative") {
        p_obs = list(ggplot2::geom_point(data = samples |>
                                      dplyr::mutate(observation = factor(observation, levels = y_axis_order)), 
                                    mapping = ggplot2::aes(x = x_axis, y = observation), 
                                    ...,
                                    color = observed_color, shape = "|",
                                    size = 10))
      } else {
        if (is_animation && ".draw" %in% colnames(samples)) {
          p_obs = list(ggplot2::geom_tile(data = samples |> 
                                         dplyr::mutate(x_axis = factor(x_axis, levels = x_axis_order),
                                                       observation = factor(observation, levels = y_axis_order)) |>
                                         dplyr::group_by(x_axis, observation, .draw) |>
                                         dplyr::count(),
                                       mapping = ggplot2::aes(x = x_axis, y = observation, fill = n),
                                       ...),
                    ggplot2::scale_fill_gradient2(name = "Count of Records", low='white', high=observed_color))
        } else {
          p_obs = list(ggplot2::geom_tile(data = samples |> 
                                         dplyr::mutate(x_axis = factor(x_axis, levels = x_axis_order),
                                                       observation = factor(observation, levels = y_axis_order)) |>
                                         dplyr::group_by(x_axis, observation) |>
                                         dplyr::count(),
                                       mapping = ggplot2::aes(x = x_axis, y = observation, fill = n),
                                       ...),
                    ggplot2::scale_fill_gradient2(name = "Count of Records", low='white', high=observed_color))
        }
      }
    } else {
      if (y_type == "quantitative") {
        p_obs = ggplot2::geom_density(data = samples, 
                                      mapping = ggplot2::aes(x = observation), 
                                      ...,
                                      color = observed_color)
      } else {
        y_lim[[1]] = 0
        p_obs = ggplot2::geom_bar(data = samples |> 
                                    dplyr::mutate(observation = factor(observation, levels = y_axis_order)) |>
                                    dplyr::count(observation) |> dplyr::ungroup(),
                                  mapping = ggplot2::aes(x = observation, y = n / sum(n)),
                                  ...,
                                  color = observed_color,
                                  fill = observed_color,
                                  stat = "identity",
                                  alpha = 0.5)
      }
    }
    p_obs = c(p_obs, ggplot2::lims(x = x_lim, y = y_lim))
    p_obs = c(p_obs,
      ggplot2::facet_grid(rows = row_vars, cols = col_vars, labeller = ggplot2::label_both))
    return(p_obs)
  }
}