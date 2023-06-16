
#' Define comparative layout in model check visualization
#'
#' @param comparative_layout Which comparative layout to use? If `"jux"`, `mcplot()`
#'  puts geoms for data observations and geoms for model predictions side by side;
#'  if `"sup"`, `mcplot()` overlays them; if `"exp"`, `mcplot()` will show comparison by
#'  explicit-encoding depending on `exp_op`
#' @param ... Other arguments passed on to [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/articles/plot_grid.html),
#'  if using `"jux"` for comparative_layout.
#' @param exp_op What operation needs for explicit-encoding? If `exp_op` is:
#'  * `"residual"`, then `mcplot()` will show the comparison between data observations and
#'  model predictions by a residual plot.
#'  * `"qq"`, then `mcplot()` will show the comparison by a Q-Q plot.
#'  * A function, then `mcplot()` will transform the data by `exp_op`.
#'
#' @export
#'
#' @examples
mc_comp_layout = function(comparative_layout, ..., exp_op = NULL) {
  p = function(mc_setting = NULL) {
    if (comparative_layout == "jux") {
      comp_layout = purrr::partial(comp_layout_jux, ...)
    } else if (comparative_layout == "sup") {
      comp_layout = purrr::partial(comp_layout_sup, ...)
    } else if (comparative_layout == "exp") {
      comp_layout = purrr::partial(comp_layout_exp, ...)
    } else if (comparative_layout == "nestjux") {
      comp_layout = purrr::partial(comp_layout_nestjux, ...)
    }
    # TODO: multiple layouts to support
    c(list(comparative_layout = comp_layout, explicit_operation = exp_op),
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
