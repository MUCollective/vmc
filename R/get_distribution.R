
# mc_get_distribution <- function(model, distribution,
#                                 input_data = NULL, ndraws = 500, is.transform = TRUE,
#                                 seed = NULL, re_formula = NULL) {
#   response_var = insight::find_response(model$formula)[1]
#
#   fit_data <- model$data
#
#   if (is.null(input_data)) {
#     input_data <- fit_data
#   }
#
#   y_label = response_var
#   x_label = NULL
#
#   if (distribution == "predictive") {
#     samples <- model %>%
#       tidybayes::predicted_draws(newdata = input_data,
#                       ndraws = ndraws,
#                       seed = seed,
#                       re_formula = re_formula) %>%
#       dplyr::mutate(prediction = .prediction)
#     samples$observation = samples[[response_var]]
#   } else {
#     samples <- model %>%
#       tidybayes::linpred_draws(newdata = input_data,
#                     dpar = distribution,
#                     transform = is.transform,
#                     ndraws = ndraws,
#                     seed = seed,
#                     re_formula = re_formula)
#     samples$prediction = samples[[distribution]]
#     samples$observation = samples[[response_var]]
#   }
#   list(samples, model, response_var, list(y = y_label, x = x_label))
# }

mc_get_distribution <- function(distribution = "predictive", newdata = NULL,
                                draw_function = NULL, response_var = NULL,
                                ndraws = 500, ...) {
  function(model) {
    if (is.null(response_var)) {
      response_var = insight::find_response(model$formula)[1]
    }

    fit_data <- model$data

    if (is.null(newdata)) {
      newdata <- fit_data
    }

    y_label = response_var
    x_label = NULL
    if (is.null(draw_function)) {
      if (distribution == "predictive") {
        samples <- model %>%
          tidybayes::predicted_draws(newdata = newdata,
                                     ndraws = ndraws,
                                     ...) %>%
          dplyr::mutate(prediction = .prediction)
      } else {
        samples <- model %>%
          tidybayes::linpred_draws(newdata = newdata,
                                   dpar = distribution,
                                   ndraws = ndraws,
                                   ...)
        samples$prediction = samples[[distribution]]
      }
    } else {
      samples <- model %>%
        draw_function(newdata = newdata,
                      ndraws = ndraws, ...)
    }
    samples$observation = samples[[response_var]]
    list(samples, model, response_var, list(y = y_label, x = x_label))
  }
}


