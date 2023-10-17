
#' Create a new mcplot
#'
#' `mcplot()` initializes a `vmc` object. It follows the ggplot syntax,
#'  which uses a plus sign (`+`) to define features of model check visualization.
#'
#' `mcplot()` uses a list of defaults to generate model check visualizations.
#'  One line of `mcplot(model)` could generate a complete visualization for
#'  posterior predictive checks. See `vignette("vmc")` for a complete
#'  guidance.
#'
#' @param model The model fit object.
#' @param observation A data frame standing for data observations. Default to be
#'  `NULL`. If `NULL`, `vmc` will set the observations as the
#'  data set that used to fit the model. The input data frame should include the variables in model formula.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' mcplot(mpg_model)
#' # note the density is on x axis and the response variable, mpg, is on y axis.
#' # But you can flip the coordinates by mc_gglayer()
#' mcplot(mpg_model) +
#'   mc_gglayer(coord_flip())
#' # you can also choose to use another observed data to show in model checks
#' new_observed_data = mtcars %>% mutate(mpg = rnorm(nrow(mtcars), 20, 5))
#' mcplot(mpg_model, new_observed_data) +
#'   mc_gglayer(coord_flip())
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
      mc_setting$conditional_vars = list(x_var = NULL, color_var = NULL, row_vars = NULL, col_vars = NULL, scales = "fixed")
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

    if (is.null(observation)) {
      observation = model$data
    }
    if (("observation_transform_group" %in% names(mc_setting))) {
      if (is.null(mc_setting$observation_transform_group)) {
        mc_setting$observation_transform_group = c(mc_setting$conditional_vars$x_var,
                                                   mc_setting$conditional_vars$color_var,
                                                   mc_setting$conditional_vars$row_vars,
                                                   mc_setting$conditional_vars$col_vars)
        if (!is.null(mc_setting$observation_transform_group)) {
          class(mc_setting$observation_transform_group) =
            c(class(mc_setting$observation_transform_group), "quosures")
        }
      } else {
        is_v_in_group_vars = function(v) {
          for (gv in mc_setting$observation_transform_group) {
            if (rlang::as_name(gv) == rlang::as_name(v)) {
              return(TRUE)
            }
          }
          return(FALSE)
        }
        for (cond_vars in c(mc_setting$conditional_vars$x_var,
                            mc_setting$conditional_vars$color_var,
                            mc_setting$conditional_vars$row_vars,
                            mc_setting$conditional_vars$col_vars)) {
          for (v in cond_vars) {
            if (!is_v_in_group_vars(v)) {
              mc_setting$observation_transform_group = c(mc_setting$observation_transform_group, v)
            }
          }
        }
      }
    }
    if (!("observation_transform" %in% names(mc_setting))) {
      transform = NULL
    } else if (!is.null(mc_setting$observation_transform)) {
      transform = function(df) {
        df %>%
          dplyr::group_by_at(mc_setting$observation_transform_group) %>%
          dplyr::mutate(observation = mc_setting$observation_transform(observation))
      }
    } else {
      transform = NULL
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
                 obs_transform = transform,
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
