
#' CDF bar plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_cdfinterval`](https://mjskay.github.io/ggdist/reference/stat_cdfinterval.html).
#' @param n_sample The number of sample of draws to show in CDF bar plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_cdf()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show samples? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all samples are collapsed together to show in one CDF bar plot; if `group_sample`
#'  is `"group"`, then each sample is shown in an individual CDF bar plot; if
#'  `group_sample` is `"hops"`, then `mc_model_cdf()` will use animation to show each
#'  sample in one frame; if `group_sample` is an function, then all samples are aggregated
#'  by `group_sample()`.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_cdf(n_sample = 50) +
#'   mc_obs_cdf() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_cdf(n_sample = 50) +
#'   mc_obs_cdf() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_cdf(n_sample = 50, group_sample = "group") +
#'   mc_obs_cdf() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_cdf(n_sample = 50, group_sample = "group", group_on = "row") +
#'   mc_obs_cdf() +
#'   mc_condition_on(x = vars(vs))
mc_model_cdf = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_cdf(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' CCDF bar plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_ccdfinterval`](https://mjskay.github.io/ggdist/reference/stat_ccdfinterval.html).
#' @param n_sample The number of sample of draws to show in CCDF bar plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_ccdf()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one CCDF bar plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual CCDF bar plot; if
#'  `group_sample` is `"hops"`, then `mc_model_ccdf()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_ccdf(n_sample = 50) +
#'   mc_obs_ccdf() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_ccdf(n_sample = 50) +
#'   mc_obs_ccdf() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_ccdf(n_sample = 50, group_sample = mean) +
#'   mc_obs_ccdf() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_ccdf(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_ccdf() +
#'   mc_condition_on(x = vars(vs))
mc_model_ccdf = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ccdf(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Dot plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_dots`](https://mjskay.github.io/ggdist/reference/stat_dots.html).
#' @param n_sample The number of sample of draws to show in Dot plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_dots()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one Dot plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual Dot plot; if
#'  `group_sample` is `"hops"`, then `mc_model_dots()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_dots(n_sample = 50) +
#'   mc_obs_dots() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_dots(n_sample = 50) +
#'   mc_obs_dots() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_dots(n_sample = 50, group_sample = mean) +
#'   mc_obs_dots() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_observation_transformation(mean) +
#'   mc_model_dots(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_reference_line() +
#'   mc_condition_on(x = vars(vs))
mc_model_dots = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dots(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Dots interval plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_dotsinterval`](https://mjskay.github.io/ggdist/reference/stat_dotsinterval.html).
#' @param n_sample The number of sample of draws to show in Dots interval plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_dotsinterval()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one Dots interval plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual Dots interval plot; if
#'  `group_sample` is `"hops"`, then `mc_model_dotsinterval()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_dotsinterval(n_sample = 50) +
#'   mc_obs_dotsinterval() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_dotsinterval(n_sample = 50) +
#'   mc_obs_dotsinterval() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_dotsinterval(n_sample = 50, group_sample = mean) +
#'   mc_obs_dotsinterval() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_dotsinterval(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_dotsinterval() +
#'   mc_condition_on(x = vars(vs))
mc_model_dotsinterval = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dotsinterval(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Eye (violin + interval) plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_eye`](https://mjskay.github.io/ggdist/reference/stat_eye.html).
#' @param n_sample The number of sample of draws to show in eye plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_eye()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one eye plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual eye plot; if
#'  `group_sample` is `"hops"`, then `mc_model_eye()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_eye(n_sample = 50) +
#'   mc_obs_eye() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_eye(n_sample = 50) +
#'   mc_obs_eye() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_eye(n_sample = 50, group_sample = mean) +
#'   mc_obs_eye() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_eye(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_eye() +
#'   mc_condition_on(x = vars(vs))
mc_model_eye = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_eye(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Half-eye (density + interval) plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_halfeye`](https://mjskay.github.io/ggdist/reference/stat_halfeye.html).
#' @param n_sample The number of sample of draws to show in half eye plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_halfeye()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one half eye plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual half eye plot; if
#'  `group_sample` is `"hops"`, then `mc_model_halfeye()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_halfeye(n_sample = 50) +
#'   mc_obs_halfeye() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_halfeye(n_sample = 50) +
#'   mc_obs_halfeye() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_halfeye(n_sample = 50, group_sample = mean) +
#'   mc_obs_halfeye() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_halfeye(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_halfeye() +
#'   mc_condition_on(x = vars(vs))
mc_model_halfeye = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_halfeye(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Slab (ridge) plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_slab`](https://mjskay.github.io/ggdist/reference/stat_slab.html).
#' @param n_sample The number of sample of draws to show in slab plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_slab()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one slab plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual slab plot; if
#'  `group_sample` is `"hops"`, then `mc_model_slab()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_slab(n_sample = 50) +
#'   mc_obs_slab() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_slab(n_sample = 50) +
#'   mc_obs_slab() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_slab(n_sample = 50, group_sample = mean) +
#'   mc_obs_slab() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_slab(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_slab() +
#'   mc_condition_on(x = vars(vs))
mc_model_slab = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_slab(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Gradient + interval plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_gradientinterval`](https://mjskay.github.io/ggdist/reference/stat_gradientinterval.html).
#' @param n_sample The number of sample of draws to show in gradient + interval plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_gradientinterval()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one gradient + interval plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual gradient + interval plot; if
#'  `group_sample` is `"hops"`, then `mc_model_gradientinterval()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_gradientinterval(n_sample = 50) +
#'   mc_obs_gradientinterval() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_gradientinterval(n_sample = 50) +
#'   mc_obs_gradientinterval() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_gradientinterval(n_sample = 50, group_sample = mean) +
#'   mc_obs_gradientinterval() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_gradientinterval(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_gradientinterval() +
#'   mc_condition_on(x = vars(vs))
mc_model_gradientinterval = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_gradient(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Histogram + interval plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_histinterval`](https://mjskay.github.io/ggdist/reference/stat_histinterval.html).
#' @param n_sample The number of sample of draws to show in histogram + interval plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_histinterval()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one histogram + interval plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual histogram + interval plot; if
#'  `group_sample` is `"hops"`, then `mc_model_histinterval()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_histinterval(n_sample = 50) +
#'   mc_obs_histinterval() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_histinterval(n_sample = 50) +
#'   mc_obs_histinterval() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_histinterval(n_sample = 50, group_sample = mean) +
#'   mc_obs_histinterval() +
#'   mc_condition_on(x = vars(vs))
#'
#' mcplot(mpg_model) +
#'   mc_model_histinterval(n_sample = 50, group_sample = mean, group_on = "row") +
#'   mc_obs_histinterval() +
#'   mc_condition_on(x = vars(vs))
mc_model_histinterval = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_his(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Point + interval plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_pointinterval`](https://mjskay.github.io/ggdist/reference/stat_pointinterval.html).
#' @param n_sample The number of sample of draws to show in pointinterval plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_pointinterval()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one pointinterval plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual pointinterval plot; if
#'  `group_sample` is `"hops"`, then `mc_model_pointinterval()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_pointinterval(n_sample = 50) +
#'   mc_obs_pointinterval() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_pointinterval(n_sample = 50) +
#'   mc_obs_pointinterval() +
#'   mc_condition_on(x = vars(disp))
mc_model_pointinterval = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_pointinterval(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Interval plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_interval`](https://mjskay.github.io/ggdist/reference/stat_interval.html).
#' @param n_sample The number of sample of draws to show in interval plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_interval()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one interval plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual interval plot; if
#'  `group_sample` is `"hops"`, then `mc_model_interval()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_interval(n_sample = 50) +
#'   mc_obs_interval() +
#'   mc_gglayer(coord_flip())
#'
#' mcplot(mpg_model) +
#'   mc_model_interval(n_sample = 50) +
#'   mc_obs_interval() +
#'   mc_condition_on(x = vars(vs))
mc_model_interval = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_interval(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Line + multiple-ribbon plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_lineribbon`](https://mjskay.github.io/ggdist/reference/stat_lineribbon.html).
#' @param n_sample The number of sample of draws to show in lineribbon plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_lineribbon()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one lineribbon plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual lineribbon plot; if
#'  `group_sample` is `"hops"`, then `mc_model_lineribbon()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_lineribbon(n_sample = 50) +
#'   mc_obs_lineribbon() +
#'   mc_condition_on(x = vars(disp))
mc_model_lineribbon = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_lineribbon(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Multiple-ribbon plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_ribbon`](https://mjskay.github.io/ggdist/reference/stat_ribbon.html).
#' @param n_sample The number of sample of draws to show in ribbon plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_ribbon()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one ribbon plot; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual ribbon plot; if
#'  `group_sample` is `"hops"`, then `mc_model_ribbon()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_ribbon(n_sample = 50) +
#'   mc_obs_ribbon() +
#'   mc_condition_on(x = vars(disp))
mc_model_ribbon = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ribbon(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Recommend a geom
#'
#' @param ... Augments passed to geom functions (e.g. [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html),
#'  [`ggplot2::geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html), and
#'  [`ggplot2::geom_tile`](https://ggplot2.tidyverse.org/reference/geom_tile.html)).
#' @param n_sample The number of sample of draws to show.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_auto()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. If `group_sample` is `"collapse"`,
#'  then all draws are collapsed together to show in one geom; if `group_sample`
#'  is `"group"`, then each draw is shown in an individual geom; if
#'  `group_sample` is `"hops"`, then `mc_model_auto()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_auto(n_sample = 50) +
#'   mc_obs_auto()
#'
#' mcplot(mpg_model) +
#'   mc_model_auto(n_sample = 50) +
#'   mc_obs_auto() +
#'   mc_condition_on(x = vars(disp))
#'
#' mcplot(mpg_model) +
#'   mc_model_auto(n_sample = 50) +
#'   mc_obs_auto() +
#'   mc_condition_on(x = vars(vs))
mc_model_auto = function(..., n_sample = NA, group_sample = NULL, group_on = NULL) {
  p = function(mc_setting = NULL) {

    uncert_rep = auto_plot(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Points geom
#'
#' @param ... Augments passed to [`ggplot2::geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html).
#' @param n_sample The number of sample of draws to show.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_point()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`. `"collapse` and `"group`
#'  are same for point geom, since point geom is just showing each data points.
#'  If `group_sample` is `"collapse"` or `"group"`,
#'  then all draws are collapsed together and each point represents one data point;
#'  if `group_sample` is `"hops"`, then `mc_model_point()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_point(n_sample = 50) +
#'   mc_obs_point() +
#'   mc_condition_on(x = vars(disp))
mc_model_point = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {

    uncert_rep = point_plot(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Line geom
#'
#' @param ... Augments passed to [`ggplot2::geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html).
#' @param n_sample The number of sample of draws to show.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_line()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"group"`.
#'  If `group_sample` is `"collapse"`, then all draws are collapsed together
#'  and are connected by one line; if `"draw"` is `"group"`, then data from each draw is
#'  connected by one line;
#'  if `group_sample` is `"hops"`, then `mc_model_line()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_model_line(n_sample = 50) +
#'   mc_obs_line() +
#'   mc_condition_on(x = vars(disp))
mc_model_line = function(..., n_sample = NA, group_sample = "group", group_on = NULL) {
  p = function(mc_setting = NULL) {

    uncert_rep = line_plot(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Tile geom
#'
#' @param ... Augments passed to [`ggplot2::geom_tile`](https://ggplot2.tidyverse.org/reference/geom_tile.html).
#' @param n_sample The number of sample of draws to show.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_tile()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"hops"`.
#'  If `group_sample` is `"collapse"`, then all draws are collapsed together
#'  and are shown in one tile plot; if `"draw"` is `"group"`, then only one draw
#'  is shown in tile plot;
#'  if `group_sample` is `"hops"`, then `mc_model_tile()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
mc_model_tile = function(..., n_sample = NA, group_sample = "hops", group_on = NULL) {
  p = function(mc_setting = NULL) {

    uncert_rep = tile_plot(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}


#' Horizontal reference lines on y axis
#'
#' @param ... Augments passed to [`ggplot2::geom_hline`](https://ggplot2.tidyverse.org/reference/geom_abline.html).
#' @param n_sample The number of sample of draws to show.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_tile()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"collapse"`.
#'  If `group_sample` is `"collapse"`, then all draws are collapsed together
#'  and are shown in one reference line; if `"draw"` is `"group"`, then only one draw
#'  is shown by reference line;
#'  if `group_sample` is `"hops"`, then `mc_model_reference_line()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mcplot(mpg_model) +
#'   mc_observation_transformation(mean) +
#'   mc_model_slab(n_sample = 50) +
#'   mc_obs_reference_line()
mc_model_reference_line = function(..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {

    uncert_rep = reference_line(..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

#' Customized geom
#'
#' @param plot The plot to use. It should be a geom function
#'  like [geoms in `ggplots`](https://ggplot2.tidyverse.org/reference/index.html#geoms)
#'  and should be able to have `data` and `mapping` augments. See examples for more details.
#' @param ... Augments passed to `plot`.
#' @param n_sample The number of sample of draws to show.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_custom()` will use all draws from
#'  posterior distribution.
#' @param group_sample How to show draws? Default `"hops"`.
#'  If `group_sample` is `"collapse"`, then all draws are collapsed together
#'  and are shown in one geom; if `"draw"` is `"group"`, then only one draw
#'  is shown in one geom;
#'  if `group_sample` is `"hops"`, then `mc_model_custom()` will use animation to show each
#'  draw in one frame; if `group_sample` is an function, then all draws are aggregated
#'  by `group_sample()`. See examples for more details.
#' @param group_on To group the samples by sample id or row id (i.e., the input data point id).
#'  Default `NULL`. If `group_on = NULL` or `group_on = "sample"`, the group sample method is applied by grouping on `.draw`.
#'  If `group_on = "row"`, the group sample method is applied on `.row`.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggdist)
#'
#' mcplot(mpg_model) +
#'   mc_model_custom(stat_boxplot, notch = TRUE) +
#'   mc_obs_custom(geom_swarm) +
#'   mc_condition_on(x = vars(vs)) +
#'   mc_gglayer(coord_flip())
mc_model_custom = function(plot, ..., n_sample = NA, group_sample = "collapse", group_on = NULL) {
  p = function(mc_setting = NULL) {

    uncert_rep = customized_plot(plot, ..., n_sample = n_sample, draw = group_sample, group_on = group_on)

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$uncertainty_representation = c(mc_setting$uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

