
mc_compare <- function(prev_ret, comparative_layout, obs_uncertainty_representation,
                       gglayers = NULL, model_color = NULL, observed_color = NULL) {
  zeallot::`%<-%`(c(samples, p_pred, labels, conditional_vars, is_animation, x_type, y_type), prev_ret)
  zeallot::`%<-%`(c(x_var, row_vars, col_vars), conditional_vars)

  x_var = x_var[[1]]

  if (is.null(observed_color)) {
    observed_color = mc_color_palettes()$observed
  }

  if (!is.null(gglayers)) {
    p_pred = p_pred + gglayers
  }

  # TODO: add nested jux comp layout for categorical data
  p = comparative_layout(obs_uncertainty_representation)(p_pred, samples, is_animation, row_vars, col_vars, "obs",
                                                         x_type, y_type, labels, gglayers, model_color, observed_color)

  p
}
