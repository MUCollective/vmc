
#' Create a new mcplot
#'
#' `mcplot()` initializes a modelcheck object. It follows the ggplot syntax,
#'  which uses a plus sign (`+`) to define features of model check visualization.
#'
#' `mcplot()` uses a list of defaults to generate model check visualizations.
#'  One line of `mcplot(model)` could generate a complete visualization for
#'  posterior predictive checks. See `vignette("modelcheck")` for a complete
#'  guidance.
#'
#' @param model The model fit object.
#' @param observation A data frame standing for data observations. Default to be
#'  `NULL`. If `NULL`, `modelcheck` will use the observations in the model fit
#'  data set. The input data frame should include the variables in model formula.
#'
#'
#' @export
#'
#' @examples
mcplot = function(model, observation = NULL) {
  p = function(mc_setting = NULL) {
    if (is.null(mc_setting)) {
      mc_setting = list()
    }

    mc = NULL

    # if (!("distribution" %in% names(mc_setting))) {
    #   mc_setting$distribution = "predictive"
    # }
    # if (!("input_data" %in% names(mc_setting))) {
    #   mc_setting$input_data = NULL
    # }
    # if (!("is.transform" %in% names(mc_setting))) {
    #   mc_setting$is.transform = TRUE
    # }
    # if (!("ndraws" %in% names(mc_setting))) {
    #   mc_setting$ndraws = 500
    # }
    # if (!("seed" %in% names(mc_setting))) {
    #   mc_setting$seed = NULL
    # }
    # if (!("re_formula" %in% names(mc_setting))) {
    #   mc_setting$re_formula = NULL
    # }
    if (!("get_distribution" %in% names(mc_setting))) {
      mc_setting$get_distribution = mc_get_distribution()
    }
    if (!("n_sample" %in% names(mc_setting))) {
      mc_setting$n_sample = 100
    }
    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting$uncertainty_representation = c(auto_plot(n_sample = NA, draw = NULL))
    }
    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting$obs_uncertainty_representation = c(auto_plot(n_sample = NA, draw = "collapse"))
    }
    if (!("comparative_layout" %in% names(mc_setting))) {
      mc_setting$comparative_layout = comp_layout_sup
    }

    if (!("conditional_vars" %in% names(mc_setting))) {
      mc_setting$conditional_vars = list(x_var = NULL, color_var = NULL, row_vars = NULL, col_vars = NULL)
    }
    if (!("gglayers" %in% names(mc_setting))) {
      mc_setting$gglayers = NULL
    }
    if (!("axis_type" %in% names(mc_setting))) {
      mc_setting$axis_type = list(x_type = NULL, y_type = NULL)
    }
    if (!("model_color" %in% names(mc_setting))) {
      mc_setting$model_color = mc_color_palettes()$model
    }
    if (!("obs_color" %in% names(mc_setting))) {
      mc_setting$obs_color = mc_color_palettes()$observed
    }
    if (!("show_draw" %in% names(mc_setting))) {
      mc_setting$show_draw = "all"
    }


    mc =
      mc_setting$get_distribution(model) %>%
      mc_operate(operation = mc_setting$explicit_operation,
                 x_label = mc_setting$x_label, y_label = mc_setting$y_label) %>%
      mc_visualize(uncertainty_representation = mc_setting$uncertainty_representation,
                   conditional_vars = mc_setting$conditional_vars,
                   n_sample = mc_setting$n_sample,
                   axis_type = mc_setting$axis_type,
                   model_color = mc_setting$model_color,
                   observed_color = mc_setting$obs_color,
                   show_draw = mc_setting$show_draw) %>%
      mc_compare(obs_data = observation,
                 comparative_layout = mc_setting$comparative_layout,
                 obs_uncertainty_representation = mc_setting$obs_uncertainty_representation,
                 gglayers = mc_setting$gglayers,
                 model_color = mc_setting$model_color,
                 observed_color = mc_setting$obs_color)

    gc()
    mc
  }
  class(p) <- 'modelcheck'
  p
}

#' @export
'+.modelcheck' = function(e1, e2) {
  f = function(mc_setting = NULL) mc_setting %>% e2() %>% e1()
  class(f) <- 'modelcheck'
  f
}

#' @export
print.modelcheck <- function(p, ...) {
  print(p())
}
