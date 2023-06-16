
#' Define the geom/uncertainty representation for data
#'
#' `mc_obs()` adds geoms for data observations, either
#'  basic geom (e.g. points, lines, and tiles) or uncertainty representations
#'  (e.g. eye plots, dots plots, and lineribbon plot). `mc_obs_*(...)` are
#'  equivalent to `mc_obs("*", ...)`.
#'
#' @param uncertainty_representation The uncertainty representation option,
#'  including ``
#' @param ... Augments passed to geom function. For example, `mc_obs("dots", size = 10)`
#'  will pass `size = 10` to the geom function [`ggdist::stat_dots()`](https://mjskay.github.io/ggdist/reference/stat_dots.html).
#'
#' @export
#'
#' @examples
mc_obs = function(obs_uncertainty_representation, ...) {
  p = function(mc_setting = NULL) {
    if (obs_uncertainty_representation == "cdf") {
      uncert_rep = uncertainty_rep_cdf(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "ccdf") {
      uncert_rep = uncertainty_rep_ccdf(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "dots") {
      uncert_rep = uncertainty_rep_dots(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "dotsinterval") {
      uncert_rep = uncertainty_rep_dotsinterval(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "eye") {
      uncert_rep = uncertainty_rep_eye(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "halfeye") {
      uncert_rep = uncertainty_rep_halfeye(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "slab") {
      uncert_rep = uncertainty_rep_slab(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "gradient") {
      uncert_rep = uncertainty_rep_gradient(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "histinterval") {
      uncert_rep = uncertainty_rep_his(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "pointinterval") {
      uncert_rep = uncertainty_rep_pointinterval(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "lineribbon") {
      uncert_rep = uncertainty_rep_lineribbon(..., draw = "collapse")
    } else if (obs_uncertainty_representation == "ribbon") {
      uncert_rep = uncertainty_rep_ribbon(..., draw = "collapse")
    }
    # else if (obs_uncertainty_representation == "none") {
    #   if (base_plot == "auto") {
    #     uncert_rep = auto_plot(..., draw = "collapse")
    #   } else if (base_plot == "point") {
    #     uncert_rep = point_plot(..., draw = "collapse")
    #   } else if (base_plot == "line") {
    #     uncert_rep = line_plot(..., draw = "collapse")
    #   } else if (base_plot == "tile") {
    #     uncert_rep = tile_plot(..., draw = "collapse")
    #   } else if (is.function(base_plot)) {
    #     uncert_rep = customized_plot(base_plot, ..., draw = "collapse")
    #   }
    # }
    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_cdf = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_cdf(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_ccdf = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ccdf(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_dots = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dots(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_dotsinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dotsinterval(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_eye = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_eye(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_halfeye = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_halfeye(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_slab = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_slab(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_gradient = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_gradient(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_histinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_his(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_pointinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_pointinterval(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_lineribbon = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_lineribbon(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_ribbon = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ribbon(..., draw = "collapse")

    if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

# mc_obs_none = function(..., base_plot = "auto") {
#   p = function(mc_setting = NULL) {
#
#     if (base_plot == "auto") {
#       uncert_rep = auto_plot(...)
#     } else if (base_plot == "point") {
#       uncert_rep = point_plot(...)
#     } else if (base_plot == "line") {
#       uncert_rep = line_plot(...)
#     } else if (base_plot == "tile") {
#       uncert_rep = tile_plot(...)
#     } else if (is.function(base_plot)) {
#       uncert_rep = customized_plot(base_plot, ...)
#     }
#
#     if (!("obs_uncertainty_representation" %in% names(mc_setting))) {
#       mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
#     } else {
#       mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
#     }
#     mc_setting
#   }
#   class(p) <- 'modelcheck'
#   p
# }


mc_obs_geom = function(..., base_plot = "auto") {
  p = function(mc_setting = NULL) {

    if (base_plot == "auto") {
      uncert_rep = auto_plot(..., draw = "collapse")
    } else if (base_plot == "point") {
      uncert_rep = point_plot(..., draw = "collapse")
    } else if (base_plot == "line") {
      uncert_rep = line_plot(..., draw = "collapse")
    } else if (base_plot == "tile") {
      uncert_rep = tile_plot(..., draw = "collapse")
    } else if (is.function(base_plot)) {
      uncert_rep = customized_plot(base_plot, ..., draw = "collapse")
    }

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_geom_auto = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = auto_plot(..., draw = "collapse")

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_geom_point = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = point_plot(..., draw = "collapse")

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_geom_line = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = line_plot(..., draw = "collapse")

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}
mc_obs_geom_tile = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = tile_plot(..., draw = "collapse")

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

mc_obs_geom_custom = function(plot, ...) {
  p = function(mc_setting = NULL) {

    uncert_rep = customized_plot(plot, ..., draw = "collapse")

    if (!("uncertainty_representation" %in% names(mc_setting))) {
      mc_setting = c(list(obs_uncertainty_representation = c(uncert_rep)), mc_setting)
    } else {
      mc_setting$obs_uncertainty_representation = c(mc_setting$obs_uncertainty_representation, uncert_rep)
    }
    mc_setting
  }
  class(p) <- 'modelcheck'
  p
}

