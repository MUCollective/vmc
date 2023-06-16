
#' Set the size of sample to show in visualization
#'
#' @param n_sample The number of sample to sample for
#'
#' @return
#' @export
#'
#' @examples
mc_sample = function(n_sample) {
  p = function(mc_setting = NULL) {
    c(list(n_sample = n_sample), mc_setting)
  }Ω
  class(p) <- 'modelcheck'
  p
}
