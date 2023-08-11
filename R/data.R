#' Miles/(US) gallon model
#'
#' An example Bayesian gaussian model fitted on mtcars dataset predicts
#' miles/(US) gallon for auotmobiles.
#'
#' @format ## `mpg_model`
#' A brms model object in Gaussian family predicting a quantitative response
#' variable `mpg` with `disp`, `vs`, `am` as predictors.
"mpg_model"

#' Manufacturer model
#'
#' An example Bayesian categorical model fitted on ggplot2::mpg dataset predicts
#' manufacturer for auotmobiles.
#'
#' @format ## `manufacturer_model`
#' A brms model object in categorical family predicting a categorical response
#' variable `manufacturer` with `cyl` as predictors.
"manufacturer_model"
