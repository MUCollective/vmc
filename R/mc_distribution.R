
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
#'  the linear/link-level predictor (i.e. push-forward transformations).
#' @param newdata Data frame to generate predictions from, or `NULL` to use the
#'  data frame used to fit model, i.e. replicated predictive distribution.
#' @param is.transform Should the linear predictor be transformed using the
#'  inverse-link function? See [`rstanarm::posterior_linpred()`](https://mc-stan.org/rstantools/reference/posterior_linpred.html) or
#'  [`brms::posterior_linpred()`](https://mc-stan.org/rstantools/reference/posterior_linpred.html).
#' @param ndraws The number of draws to return, or `NULL` to return all draws.
#' @param seed A seed to use when subsampling draws (i.e. when ndraws is not NULL).
#' @param re_formula Formula containing group-level effects to be considered
#'  in the prediction. See [`tidybayes::add_predicted_draws`](http://mjskay.github.io/tidybayes/reference/add_predicted_draws.html).
#'
#' @export
#'
#' @examples
mc_distribution = function(distribution = "predictive", newdata = NULL, is.transform = TRUE, ndraws = 500,
                           seed = NULL, re_formula = NULL) {
  p = function(mc_setting = NULL) {
    c(list(distribution = distribution, input_data = newdata, is.transform = is.transform,
           ndraws = ndraws, seed = seed, re_formula = re_formula), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
