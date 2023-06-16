
#' Pass `ggplot2::layer()` to model check visualization
#'
#' @param layers A list of `ggplot2::layer()`.
#'
#' @export
#'
#' @examples
mc_gglayer = function(layers) {
  p = function(mc_setting = NULL) {
    c(list(gglayers = layers), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}
