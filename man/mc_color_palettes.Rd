% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mc_colors.R
\name{mc_color_palettes}
\alias{mc_color_palettes}
\alias{mc_set_model_color}
\alias{mc_set_obs_color}
\title{Colors in model check visualization}
\usage{
mc_color_palettes()

mc_set_model_color(color = NULL)

mc_set_obs_color(color = NULL)
}
\arguments{
\item{color}{The color used for model predictions and data
observations. By default, \code{mcplot()} uses \code{mc_color_palettes()} to define
colors for model predictions and data observations.}
}
\description{
\code{mc_color_palettes} return a pair of default color for data observations
and model prediction.
}
\details{
\code{mc_set_model_color} sets a global color for all geoms that represent model
predictions. It change all color related aesthetics (e.g. color or fill).
\code{mc_set_obs_color} does the same thing as \code{mc_set_model_color} except setting
for geoms that represent data observations.
}
\examples{

mc_color_palettes()

mcplot(mpg_model) +
  mc_set_model_color(color = "#d95f02")
  mc_set_obs_color(color = "#1b9e77")
}
