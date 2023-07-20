
#' Define comparative layout in model check visualization
#'
#' Which comparative layout to use? If using `mc_layout_juxtaposition()`, `mcplot()`
#'  will use juxtaposition layout to puts data observations and model predictions side by side in two seperated plots;
#'  if using `mc_layout_superposition()`, `mcplot()` overlays data observations and model predictions in one plot;
#'  if using `mc_layout_encoding`, `mcplot()` will show comparison by explicit-encoding;
#'  if using `mc_layout_nested`, `mcplot()` will show comparison side by side in one plot.
#'
#' @param ... Other arguments passed on to [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/articles/plot_grid.html),
#'  when using juxtaposition.
#' @param justification A number ro set the deviate from center when using nested juxtaposition.
#'  If `justification` is a positive number, model predictions will be on the right
#'  and data observations will be on the left; if `justification` is a negative
#'  number, the positions are reversed.
#' @param transform What operation needs for explicit-encoding? If `transform` is:
#'  * `"residual"`, then `mcplot()` will show the comparison between data observations and
#'    model predictions by a residual plot.
#'  * `"qq"`, then `mcplot()` will show the comparison by a Q-Q plot.
#'  * `"worm"`, then `mcplot()` will show the comparison by a detrended Q-Q plot (worm plot).
#'  * A function, then `mcplot()` will transform the data by that function.
#'    The input of the function is a data frame generated from the `newdata` data frame
#'    passed to `mc_distribution()`, which includes a .row column (a factor
#'    grouping rows from the input newdata), .chain column (the chain each draw
#'    came from, or NA if the model does not provide chain information),
#'    .iteration column (the iteration the draw came from, or NA if the model
#'    does not provide iteration information), and a .draw column (a unique
#'    index corresponding to each draw from the distribution). The output of
#'    the function should includes a column named `y_axis` that specifies the data
#'    shown on y axis at least and an optional column named `x_axis` that specifies
#'    the data shown on x axis. If the output includes `x_axis`, `mcplot()` will
#'    ignore the conditional variable `x` defined in `mc_condition_on()`.
#'    See examples for more details.
#'
#' @export
#'
#' @examples
mc_layout_juxtaposition = function(...) {
  p = function(mc_setting = NULL) {
    comp_layout = purrr::partial(comp_layout_jux, ...)

    c(list(comparative_layout = comp_layout, explicit_operation = NULL),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

#' @rdname mc_layout_juxtaposition
#' @export
mc_layout_superposition = function() {
  p = function(mc_setting = NULL) {
    comp_layout = comp_layout_sup

    c(list(comparative_layout = comp_layout, explicit_operation = NULL),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

#' @rdname mc_layout_juxtaposition
#' @export
mc_layout_nested = function(justification = .2) {
  p = function(mc_setting = NULL) {
    comp_layout = purrr::partial(comp_layout_nestjux, justification = justification)

    c(list(comparative_layout = comp_layout, explicit_operation = NULL),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

#' @rdname mc_layout_juxtaposition
#' @export
mc_layout_encoding = function(transform) {
  p = function(mc_setting = NULL) {
    comp_layout = comp_layout_exp

    c(list(comparative_layout = comp_layout, explicit_operation = transform),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}


# mc_comp_layout = function(comparative_layout, ..., exp_op = NULL) {
#   p = function(mc_setting = NULL) {
#     if (comparative_layout == "jux") {
#       comp_layout = purrr::partial(comp_layout_jux, ...)
#     } else if (comparative_layout == "sup") {
#       comp_layout = purrr::partial(comp_layout_sup, ...)
#     } else if (comparative_layout == "exp") {
#       comp_layout = purrr::partial(comp_layout_exp, ...)
#     } else if (comparative_layout == "nestjux") {
#       comp_layout = purrr::partial(comp_layout_nestjux, ...)
#     }
#
#     c(list(comparative_layout = comp_layout, explicit_operation = exp_op),
#       mc_setting)
#   }
#   class(p) <- 'modelcheck'
#   p
# }
