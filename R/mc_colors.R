
#' Colors in model check visualization
#'
#' `mc_color_palettes` return a pair of default color for data observations
#'  and model prediction.
#'
#' `mc_set_model_color` sets a global color for all geoms that represent model
#'  predictions. It change all color related aesthetics (e.g. color or fill).
#'  `mc_set_obs_color` does the same thing as `mc_set_model_color` except setting
#'  for geoms that represent data observations.
#'
#' @param model_color,obs_color The color used for model predictions and data
#'  observations. By default, `mcplot()` uses `mc_color_palettes()` to define
#'  colors for model predictions and data observations.
#'
#' @export
#'
#' @examples
mc_color_palettes = function() { list(observed = "#4d7aa8", model = "#f18e28") }

#' @rdname mc_color_palettes
#' @export
mc_set_model_color = function(model_color = NULL) {
  p = function(mc_setting = NULL) {
    c(list(model_color = model_color), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

#' @rdname mc_color_palettes
#' @export
mc_set_obs_color = function(obs_color = NULL) {
  p = function(mc_setting = NULL) {
    c(list(obs_color = obs_color), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
