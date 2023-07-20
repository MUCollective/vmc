
mc_model = function(uncertainty_representation, ...) {
  p = function(mc_setting = NULL) {
    if (uncertainty_representation == "cdf") {
      uncert_rep = uncertainty_rep_cdf(...)
    } else if (uncertainty_representation == "dots") {
      uncert_rep = uncertainty_rep_dots(...)
    } else if (uncertainty_representation == "eye") {
      uncert_rep = uncertainty_rep_eye(...)
    } else if (uncertainty_representation == "gradient") {
      uncert_rep = uncertainty_rep_gradient(...)
    } else if (uncertainty_representation == "histinterval") {
      uncert_rep = uncertainty_rep_his(...)
    } else if (uncertainty_representation == "pointinterval") {
      uncert_rep = uncertainty_rep_pointinterval(...)
    } else if (uncertainty_representation == "interval") {
      uncert_rep = uncertainty_rep_interval(...)
    } else if (uncertainty_representation == "lineribbon") {
      uncert_rep = uncertainty_rep_lineribbon(...)
    }
    # else if (uncertainty_representation == "none") {
    #   if (base_plot == "auto") {
    #     uncert_rep = auto_plot(...)
    #   } else if (base_plot == "point") {
    #     uncert_rep = point_plot(...)
    #   } else if (base_plot == "line") {
    #     uncert_rep = line_plot(...)
    #   } else if (base_plot == "tile") {
    #     uncert_rep = tile_plot(...)
    #   } else if (is.function(base_plot)) {
    #     uncert_rep = customized_plot(base_plot, ...)
    #   }
    # }
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


#' CDF bar plot for model predictions
#'
#' @param ... Augments passed to [`ggdist::stat_cdfinterval`](https://mjskay.github.io/ggdist/reference/stat_cdfinterval.html).
#' @param n_sample The number of sample of draws to show in CDF bar plot.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_cdf()` will use all draws from
#'  posterior distribution.
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one CDF bar plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual CDF bar plot; if
#'  `draw` is `"hops"`, then `mc_model_cdf()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_cdf = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_cdf(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one CCDF bar plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual CCDF bar plot; if
#'  `draw` is `"hops"`, then `mc_model_ccdf()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_ccdf = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ccdf(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one Dot plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual Dot plot; if
#'  `draw` is `"hops"`, then `mc_model_dots()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_dots = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dots(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one Dots interval plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual Dots interval plot; if
#'  `draw` is `"hops"`, then `mc_model_dotsinterval()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_dotsinterval = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dotsinterval(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one eye plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual eye plot; if
#'  `draw` is `"hops"`, then `mc_model_eye()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_eye = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_eye(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one half eye plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual half eye plot; if
#'  `draw` is `"hops"`, then `mc_model_halfeye()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_halfeye = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_halfeye(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one slab plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual slab plot; if
#'  `draw` is `"hops"`, then `mc_model_slab()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_slab = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_slab(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one gradient + interval plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual gradient + interval plot; if
#'  `draw` is `"hops"`, then `mc_model_gradientinterval()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_gradientinterval = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_gradient(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one histogram + interval plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual histogram + interval plot; if
#'  `draw` is `"hops"`, then `mc_model_histinterval()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_histinterval = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_his(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one pointinterval plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual pointinterval plot; if
#'  `draw` is `"hops"`, then `mc_model_pointinterval()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_pointinterval = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_pointinterval(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one interval plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual interval plot; if
#'  `draw` is `"hops"`, then `mc_model_interval()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_interval = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_interval(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one lineribbon plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual lineribbon plot; if
#'  `draw` is `"hops"`, then `mc_model_lineribbon()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_lineribbon = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_lineribbon(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one ribbon plot; if `draw`
#'  is `"group"`, then each draw is shown in an individual ribbon plot; if
#'  `draw` is `"hops"`, then `mc_model_ribbon()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_ribbon = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ribbon(..., n_sample = n_sample, draw = draw)

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

mc_model = function(..., base_plot = "auto") {
  p = function(mc_setting = NULL) {

    if (base_plot == "auto") {
      uncert_rep = auto_plot(...)
    } else if (base_plot == "point") {
      uncert_rep = point_plot(...)
    } else if (base_plot == "line") {
      uncert_rep = line_plot(...)
    } else if (base_plot == "tile") {
      uncert_rep = tile_plot(...)
    } else if (is.function(base_plot)) {
      uncert_rep = customized_plot(base_plot, ...)
    }

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
#' @param draw How to show draws? Default `"collapse"`. If `draw` is `"collapse"`,
#'  then all draws are collapsed together to show in one geom; if `draw`
#'  is `"group"`, then each draw is shown in an individual geom; if
#'  `draw` is `"hops"`, then `mc_model_auto()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_auto = function(..., n_sample = NA, draw = NULL) {
  p = function(mc_setting = NULL) {

    uncert_rep = auto_plot(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"collapse"`. `"collapse` and `"group`
#'  are same for point geom, since point geom is just showing each data points.
#'  If `draw` is `"collapse"` or `"group"`,
#'  then all draws are collapsed together and each point represents one data point;
#'  if `draw` is `"hops"`, then `mc_model_point()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_point = function(..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {

    uncert_rep = point_plot(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"group"`.
#'  If `draw` is `"collapse"`, then all draws are collapsed together
#'  and are connected by one line; if `"draw"` is `"group"`, then data from each draw is
#'  connected by one line;
#'  if `draw` is `"hops"`, then `mc_model_line()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_line = function(..., n_sample = NA, draw = "group") {
  p = function(mc_setting = NULL) {

    uncert_rep = line_plot(..., n_sample = n_sample, draw = draw)

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
#' @param draw How to show draws? Default `"hops"`.
#'  If `draw` is `"collapse"`, then all draws are collapsed together
#'  and are shown in one tile plot; if `"draw"` is `"group"`, then only one draw
#'  is shown in tile plot;
#'  if `draw` is `"hops"`, then `mc_model_tile()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_tile = function(..., n_sample = NA, draw = "hops") {
  p = function(mc_setting = NULL) {

    uncert_rep = tile_plot(..., n_sample = n_sample, draw = draw)

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
#' @param geom_plot The geom plot to use. `"geom_plot"` should be a geom function
#'  like [geoms in `ggplots`](https://ggplot2.tidyverse.org/reference/index.html#geoms)
#'  and should be able to have `data` and `mapping` augments. See examples for more details.
#' @param ... Augments passed to `geom_plot`.
#' @param n_sample The number of sample of draws to show.
#'  Default `NA`. If `n_sample` is `NA`, then `mc_model_custom()` will use all draws from
#'  posterior distribution.
#' @param draw How to show draws? Default `"hops"`.
#'  If `draw` is `"collapse"`, then all draws are collapsed together
#'  and are shown in one geom; if `"draw"` is `"group"`, then only one draw
#'  is shown in one geom;
#'  if `draw` is `"hops"`, then `mc_model_custom()` will use animation to show each
#'  draw in one frame; if `draw` is an function, then all draws are aggregated
#'  by `draw()`. See examples for more details.
#'
#' @export
#'
#' @examples
mc_model_custom = function(geom_plot, ..., n_sample = NA, draw = "collapse") {
  p = function(mc_setting = NULL) {

    uncert_rep = customized_plot(geom_plot, ..., n_sample = n_sample, draw = draw)

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

