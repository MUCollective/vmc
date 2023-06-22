
mc_get_distribution <- function(model, distribution,
                                input_data = NULL, ndraws = 500, is.transform = TRUE,
                                seed = NULL, re_formula = NULL) {
  response_var = insight::find_response(model$formula)[1]

  fit_data <- model$data

  if (is.null(input_data)) {
    input_data <- fit_data
  }

  y_label = response_var
  x_label = NULL

  if (distribution == "predictive") {
    samples <- model %>%
      tidybayes::predicted_draws(newdata = input_data,
                      ndraws = ndraws,
                      seed = seed,
                      re_formula = re_formula) %>%
      dplyr::mutate(prediction = .prediction)
    samples$observation = samples[[response_var]]
  } else {
    samples <- model %>%
      tidybayes::linpred_draws(newdata = input_data,
                    dpar = distribution,
                    transform = is.transform,
                    ndraws = ndraws,
                    seed = seed,
                    re_formula = re_formula)
    samples$prediction = samples[[distribution]]
    samples$observation = samples[[response_var]]
  }
  list(samples, model, response_var, list(y = y_label, x = x_label))
}
