
mc_compare <- function(prev_ret, obs_data, obs_transform, comparative_layout, obs_uncertainty_representation,
                       gglayers = NULL, model_color = NULL, observed_color = NULL) {
  zeallot::`%<-%`(c(samples, p_pred, labels, response_var, conditional_vars, is_animation, x_type, y_type), prev_ret)
  zeallot::`%<-%`(c(x_var, color_var, row_vars, col_vars), conditional_vars)

  x_var = x_var[[1]]

  if (is.null(obs_data)) {
    obs_data = samples
  } else {
    obs_data = obs_data %>% dplyr::mutate(observation = !!as.name(response_var))
    if (!is.null(x_var)) {
      obs_data = obs_data %>% dplyr::mutate(x_axis = !!x_var)
    }
  }

  if (!is.null(obs_transform)) {
    obs_data = obs_transform(obs_data %>% ungroup())
  }

  if ("x_axis" %in% colnames(obs_data) && x_type != "quantitative" && !is.factor(obs_data$x_axis)) {
    obs_data = obs_data %>% dplyr::mutate(x_axis = factor(x_axis, levels = sort(unique(x_axis))))
  }

  if (is.null(observed_color)) {
    observed_color = mc_color_palettes()$observed
  }

  if (!is.null(gglayers)) {
    p_pred = p_pred + gglayers
  }

  if (is.null(color_var)) {
    obs_color = "obs"
  } else {
    obs_color = color_var[[1]]
  }

  p = comparative_layout(
    obs_uncertainty_representation
    )(p_pred, obs_data, is_animation,
      color_var, row_vars, col_vars,
      obs_color, x_type, y_type, labels,
      gglayers, model_color, observed_color)

  p
}
