
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

mc_get_distribution <- function(distribution = "prediction", newdata = NULL,
                                draw_function = NULL, response_var = NULL,
                                ndraws = 500, ...) {
  function(model) {
    x_label = NULL

    if (is.data.frame(model)) {  # code for testing
      response_var = "mpg"
      y_label = response_var
      if (distribution == "prediction") {
        samples <- model %>%
          dplyr::mutate(prediction = .prediction)
      } else {
        samples <- model
        samples$prediction = samples[[distribution]]
      }
    } else {
      if (is.null(response_var)) {
        response_var = insight::find_response(model)
      }
      y_label = response_var
      fit_data <- insight::get_data(model)

      if (is.null(newdata)) {
        newdata <- fit_data
      }

      if (is.null(draw_function)) {
        if (distribution == "prediction") {
          if(inherits(model, c("brmsfit", "stanreg", "stanmvreg"))) {
            samples <- model %>%
              tidybayes::predicted_draws(newdata = newdata,
                                         ndraws = ndraws,
                                         ...) %>%
              dplyr::mutate(prediction = .prediction)
          } else {
            samples = get_predicted(model,
                                        data = newdata,
                                        predict = "prediction",
                                        iterations = ndraws) %>%
              as.data.frame() %>%
              cbind(newdata) %>%
              bayestestR::reshape_iterations() %>%
              mutate(prediction = iter_value,
                     .row = iter_index,
                     .draw = iter_group)
            #   mutate(.row = row_number()) %>%
            #   tidyr::unite("prediction", contains("iter_")) %>%
            #   rowwise() %>%
            #   mutate(prediction = list(as.numeric(unlist(strsplit(prediction, "_")))),
            #          .draw = list(1:ndraws))
            #
            # samples <- insight::get_data(model) %>%
            #   cbind(predictions) %>%
            #   tidyr::unnest(c(prediction, .draw))
          }
        } else {
          if(inherits(model, c("brmsfit", "stanreg", "stanmvreg"))) {
            samples <- model %>%
              tidybayes::linpred_draws(newdata = newdata,
                                       dpar = distribution,
                                       ndraws = ndraws,
                                       ...)
            samples$prediction = samples[[distribution]]
          } else {
            samples = get_predicted(model,
                                    data = newdata,
                                    predict = distribution,
                                    iterations = ndraws) %>%
              as.data.frame() %>%
              cbind(newdata) %>%
              bayestestR::reshape_iterations() %>%
              mutate(prediction = iter_value,
                     .row = iter_index,
                     .draw = iter_group)
            # predictions = get_predicted(model,
            #                             data = newdata,
            #                             predict = distribution,
            #                             iterations = ndraws) %>%
            #   as.data.frame() %>%
            #   mutate(.row = row_number()) %>%
            #   tidyr::unite("prediction", contains("iter_")) %>%
            #   rowwise() %>%
            #   mutate(prediction = list(as.numeric(unlist(strsplit(prediction, "_")))),
            #          .draw = list(1:ndraws))
            #
            # samples <- insight::get_data(model) %>%
            #   cbind(predictions) %>%
            #   tidyr::unnest(c(prediction, .draw))
          }
        }
      } else {
        samples <- model %>%
          draw_function(newdata = newdata,
                        ndraws = ndraws, ...)
      }
    }
    samples$observation = samples[[response_var]]
    list(samples, model, response_var, list(y = y_label, x = x_label))
  }
}


