
mc_visualize <- function(prev_ret, uncertainty_representation,
                         conditional_vars = list(NULL, NULL, NULL, NULL),
                         n_sample = 100,
                         axis_type = list(NULL, NULL),
                         model_color = NULL,
                         observed_color = NULL,
                         show_draw = "all") {
  zeallot::`%<-%`(c(samples, response_var, labels), prev_ret)
  zeallot::`%<-%`(c(x_var, color_var, row_vars, col_vars), conditional_vars)
  zeallot::`%<-%`(c(x_axis_type, y_axis_type), axis_type)

  x_var = x_var[[1]]
  if (!is.null(x_var)) {
    samples = samples %>% dplyr::select(x_axis = !!x_var, everything())
    labels$x = rlang::quo_name(x_var)
  }

  if (!("y_axis" %in% colnames(samples))) {
    samples = samples %>% dplyr::select(y_axis = prediction, everything())
  }

  # sampling_by <- function(distribution, number_of_total, number_of_sample, way = "uniform") {
  #   nrow <- max(distribution$.row)
  #   if (way == "uniform") {
  #     sample_rank <-
  #       ceiling(matrix(runif(nrow * number_of_sample), ncol=number_of_sample) * number_of_total)
  #   } else {
  #     sample_rank <-
  #       ceiling(torus(nrow, number_of_sample) * number_of_total)
  #   }
  #
  #   samples <- distribution %>%
  #     dplyr::group_by(.row) %>%
  #     dplyr::mutate(ranks_in_row = order(order(y_axis, decreasing=FALSE))) %>%
  #     dplyr::filter(ranks_in_row %in% sample_rank[.row,]) %>%
  #     dplyr::mutate(sample_id = seq_along(.draw))
  #
  #   samples
  # }

  # samples <- samples %>%
  # sampling_by(ndraw, nHOPs_draw, way = "quasi")
  # sample_ids = sample(1:ndraw, n_sample)
  # samples <- samples %>%
  #   dplyr::filter(.draw %in% sample_ids)

  # sample_ids = sample(1:ndraw)

  if ("y_axis" %in% colnames(samples) && is.null(y_axis_type)) {
    y_axis_type = samples$y_axis %>% get_type()
  }
  if ("x_axis" %in% colnames(samples) && is.null(x_axis_type)) {
    x_axis_type = samples$x_axis %>% get_type()
  }

  colors_legend = c("obs" = observed_color, "model" = model_color)

  if ("x_axis" %in% colnames(samples)) {
    if (x_axis_type == "quantitative" || is.factor(samples$x_axis)) {
      p <- ggplot2::ggplot(data = samples, mapping = ggplot2::aes(x = x_axis))
    } else {
      p <- ggplot2::ggplot(data = samples, mapping = ggplot2::aes(x = factor(x_axis, levels = sort(unique(x_axis)))))
    }
  } else {
    p <- ggplot2::ggplot()
  }
  if (is.null(color_var)) {
    p <- p + ggplot2::scale_color_manual(name = "color", values = colors_legend) +
      ggplot2::scale_fill_manual(name = "fill", values = colors_legend)
  }

  if (is.null(color_var)) {
    model_color = "model"
  } else {
    model_color = color_var[[1]]
  }

  call_rep = function(func) {
    func(samples,
         row_vars,
         col_vars,
         labels,
         list(x_axis_type, y_axis_type),
         model_color,
         FALSE,
         rlang::quo(y_axis))
  }

  for (uncert_rep in uncertainty_representation) {
    if (is.function(uncert_rep)) {
      p <- p +
        call_rep(uncert_rep)
    }
  }
  if ("x_axis" %in% colnames(samples)) {
    p <- p + ggplot2::labs(x = labels$x, y = labels$y)
  } else {
    p <- p + ggplot2::labs(y = labels$y)
  }
  is_animation = "gganim" %in% class(p)

  p <- p + ggplot2::facet_grid(rows = row_vars, cols = col_vars, labeller = ggplot2::label_both)

  list(samples, p, labels, response_var, conditional_vars, is_animation, x_axis_type, y_axis_type)
}
