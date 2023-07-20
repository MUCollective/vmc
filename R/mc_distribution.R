
#' Define how to draw from posterior distribution
#'
#' `modelcheck` uses the R package [`tidybayes`](http://mjskay.github.io/tidybayes/index.html)
#'  to draw from posterior distributions. For posterior predictive distributions,
#'  `modelcheck` uses `tidybayes::predicted_draws()`; for posterior distribution
#'  of push-forward transformation, `modelcheck` uses `tidybayes::linpred_draws()`.
#'
#' @param distribution Which distribution to draw from. The options include `"predictive"` and
#'  and push-forward transformations (e.g. `mu`, `sigma`, and `phi`). For example,
#'  if the model is normal family and `distribution = "predictive"`, we draws from
#'  posterior predictive distribution; if `distribution = "mu"`, we draws from
#'  the distribution of linear/link-level predictor (i.e. push-forward transformations).
#' @param newdata Data frame to generate predictions from, or `NULL` to reuse the
#'  data frame used to fit model, i.e. replicated predictive distribution.
#' @param draw_function The function used to draw from model's posterior distributions.
#'  Default to be `NULL`. If `draw_function` is `NULL`, then `modelcheck` will
#'  use `tidybayes::predicted_draws()` for `distribution = "predictive"` and
#'  use `tidybayes::linpred_draws()` for other distributions of
#'  linear/link-level predictors. If `draw_function` is not `NULL`, it should be
#'  a function that takes `model`, `newdata`, and `ndraws` as inputs and output a data
#'  frame that sampled from `model` on `newdata` with at least a `prediction`
#'  column (the draws from the model's distribution), a `.row` column (a factor grouping rows from
#'  the input newdata), and a `.draw` column (a unique index corresponding to
#'  each draw from the distribution).
#' @param response_var A string for the response variable in model. Default to
#'  be `NULL`. If `NULL`, `modelcheck` will infer the response variable from
#'  `model$formula`.
#' @param ... Augments passed to `draw_function`. If `draw_function` is `NULL`,
#'  then `...` will be passed to `tidybayes::predicted_draws()` or `tidybayes::linpred_draws()`
#' @param ndraws The number of draws to return, or `NULL` to return all draws.
#' @export
#'
#' @examples
#' library(brms)
#' library(modelr)
#' model = brm(
#'   bf(mpg ~ disp),
#'   init = "0",
#'   data = mtcars,
#'   iter = 6000,
#' )
#' mcplot(model) +
#'   mc_distribution()
#'
#' mcplot(model) +
#'   mc_distribution("mu")
#'
#' mcplot(model) +
#'   mc_distribution("mu", data_grid(model$data, disp, carb, vs, am, mpg))
#'
#' library(tidybayes)
#' library(dplyr)
#' draw_function = function(model, newdata, ...) {
#'   epred_draws(model, newdata, dpar = "mu", ...) %>%
#'     mutate(prediction = mu)
#' }
#'
#' mcplot(model) +
#'   mc_distribution(draw_function = draw_function)
mc_distribution = function(distribution = "predictive", newdata = NULL, draw_function = NULL, response_var = NULL, ndraws = 500, ...) {
  p = function(mc_setting = NULL) {
    c(list(get_distribution = mc_get_distribution(distribution = distribution, newdata = newdata,
                                                  draw_function = draw_function, response_var = response_var,
                                                  ndraws = ndraws,
                                                  ...)), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
