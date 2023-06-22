
mc_obs = function(obs_uncertainty_representation, ...) {
  p = function(mc_setting = NULL) {
    if (obs_uncertainty_representation == "cdf") {
      uncert_rep = uncertainty_rep_cdf(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "ccdf") {
      uncert_rep = uncertainty_rep_ccdf(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "dots") {
      uncert_rep = uncertainty_rep_dots(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "dotsinterval") {
      uncert_rep = uncertainty_rep_dotsinterval(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "eye") {
      uncert_rep = uncertainty_rep_eye(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "halfeye") {
      uncert_rep = uncertainty_rep_halfeye(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "slab") {
      uncert_rep = uncertainty_rep_slab(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "gradient") {
      uncert_rep = uncertainty_rep_gradient(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "histinterval") {
      uncert_rep = uncertainty_rep_his(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "pointinterval") {
      uncert_rep = uncertainty_rep_pointinterval(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "lineribbon") {
      uncert_rep = uncertainty_rep_lineribbon(..., n_sample = NA, draw = "collapse")
    } else if (obs_uncertainty_representation == "ribbon") {
      uncert_rep = uncertainty_rep_ribbon(..., n_sample = NA, draw = "collapse")
    }
    # else if (obs_uncertainty_representation == "none") {
    #   if (base_plot == "auto") {
    #     uncert_rep = auto_plot(..., n_sample = NA, draw = "collapse")
    #   } else if (base_plot == "point") {
    #     uncert_rep = point_plot(..., n_sample = NA, draw = "collapse")
    #   } else if (base_plot == "line") {
    #     uncert_rep = line_plot(..., n_sample = NA, draw = "collapse")
    #   } else if (base_plot == "tile") {
    #     uncert_rep = tile_plot(..., n_sample = NA, draw = "collapse")
    #   } else if (is.function(base_plot)) {
    #     uncert_rep = customized_plot(base_plot, ..., n_sample = NA, draw = "collapse")
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

#' @rdname mc_model_cdf
#' @export
mc_obs_cdf = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_cdf(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_ccdf
#' @export
mc_obs_ccdf = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ccdf(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_dots
#' @export
mc_obs_dots = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dots(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_dotsinterval
#' @export
mc_obs_dotsinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dotsinterval(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_eye
#' @export
mc_obs_eye = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_eye(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_halfeye
#' @export
mc_obs_halfeye = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_halfeye(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_slab
#' @export
mc_obs_slab = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_slab(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_gradient
#' @export
mc_obs_gradient = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_gradient(..., n_sample = NA, draw = "collapse")

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


#' @rdname mc_model_histinterval
#' @export
mc_obs_histinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_his(..., n_sample = NA, draw = "collapse")

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


#' @rdname mc_model_pointinterval
#' @export
mc_obs_pointinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_pointinterval(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_interval
#' @export
mc_obs_interval = function(..., scale_color = ggplot2::scale_color_brewer(palette = 1)) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_interval(..., scale_fill = scale_color, n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_lineribbon
#' @export
mc_obs_lineribbon = function(..., scale_fill = ggplot2::scale_fill_brewer(palette = 1)) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_lineribbon(..., scale_fill = scale_fill, n_sample = NA, draw = "collapse")

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


#' @rdname mc_model_ribbon
#' @export
mc_obs_ribbon = function(..., scale_fill = ggplot2::scale_fill_brewer(palette = 1)) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ribbon(..., scale_fill = scale_fill, n_sample = NA, draw = "collapse")

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


mc_obs = function(..., base_plot = "auto") {
  p = function(mc_setting = NULL) {

    if (base_plot == "auto") {
      uncert_rep = auto_plot(..., n_sample = NA, draw = "collapse")
    } else if (base_plot == "point") {
      uncert_rep = point_plot(..., n_sample = NA, draw = "collapse")
    } else if (base_plot == "line") {
      uncert_rep = line_plot(..., n_sample = NA, draw = "collapse")
    } else if (base_plot == "tile") {
      uncert_rep = tile_plot(..., n_sample = NA, draw = "collapse")
    } else if (is.function(base_plot)) {
      uncert_rep = customized_plot(base_plot, ..., n_sample = NA, draw = "collapse")
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


#' @rdname mc_model_auto
#' @export
mc_obs_auto = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = auto_plot(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_point
#' @export
mc_obs_point = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = point_plot(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_line
#' @export
mc_obs_line = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = line_plot(..., n_sample = NA, draw = "collapse")

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

#' @rdname mc_model_tile
#' @export
mc_obs_tile = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = tile_plot(..., n_sample = NA, draw = "collapse")

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


#' @rdname mc_model_custom
#' @export
mc_obs_custom = function(plot, ...) {
  p = function(mc_setting = NULL) {

    uncert_rep = customized_plot(plot, ..., n_sample = NA, draw = "collapse")

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

