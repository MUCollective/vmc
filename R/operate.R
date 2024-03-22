
mc_operate <- function(prev_ret, operation, x_label = NULL, y_label = NULL) {
  zeallot::`%<-%`(c(samples, model, response_var, labels), prev_ret)


  ndraw <- max(samples$.draw)
  n.row <- max(samples$.row)
  if (is.null(operation)) {
    return(list(samples, response_var, labels))
  }
  if (is.function(operation)) {
    samples = operation(samples %>% ungroup())
    labels$x = x_label
    labels$y = y_label
  } else if (operation == "residual") {
    samples = samples %>%
      ungroup() %>%
      dplyr::mutate(y_axis = prediction - observation)
    labels$y = paste("prediction - observation:", labels$y)
  } else if (operation == "qq") {
    samples <- samples %>%
      dplyr::ungroup() %>%
      dplyr::select(-.row, -.draw, -.chain, -.iteration) %>%
      tidybayes::add_predicted_draws(model, value = ".newprediction", ndraws = 100) %>%
      dplyr::summarise(y_axis = qnorm(mean(.newprediction < observation))) %>%
      dplyr::select(-.row) %>%
      merge(samples) %>%
      # dplyr::group_by(.draw) %>%
      dplyr::mutate(ranks_in_row = order(order(y_axis, decreasing=FALSE)),
             x_axis = qnorm((ranks_in_row) / dplyr::n()))
    labels$x = "theoretical"
    labels$y = paste("prediction < observation:", labels$y)
  } else if (operation == "worm") {
    samples <- samples %>%
      dplyr::ungroup() %>%
      dplyr::select(-.row, -.draw, -.chain, -.iteration) %>%
      tidybayes::add_predicted_draws(model, value = ".newprediction", ndraws = 100) %>%
      dplyr::summarise(y_axis = qnorm(mean(.newprediction < observation))) %>%
      dplyr::select(-.row) %>%
      merge(samples) %>%
      dplyr::group_by(.draw) %>%
      dplyr::mutate(ranks_in_row = order(order(y_axis, decreasing=FALSE)),
             x_axis = qnorm((ranks_in_row) / n.row),
             y_axis = y_axis - x_axis)
    labels$x = "theoretical"
    labels$y = paste(labels$y, "deviation")
  }
  list(samples, response_var, labels)
}
