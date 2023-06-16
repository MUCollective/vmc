
#' Define the geom/uncertainty representation for model
#'
#' `mc_model()` adds geoms for draws from model posterior distribution, either
#'  basic geom (e.g. points, lines, and tiles) or uncertainty representations
#'  (e.g. eye plots, dots plots, and lineribbon plot). `mc_model_*(...)` are
#'  equivalent to `mc_model("*", ...)`.
#'
#' @param uncertainty_representation The uncertainty representation option,
#'  including ``
#' @param ... Augments passed to geom function. For example, `mc_model("dots", size = 10)`
#'  will pass `size = 10` to the geom function [`ggdist::stat_dots()`](https://mjskay.github.io/ggdist/reference/stat_dots.html).
#'
#' @export
#'
#' @examples
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


#' CDF bar plot
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
mc_model_cdf = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_cdf(...)

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

#' @rdname mc_model
#' @export
mc_model_ccdf = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ccdf(...)

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

#' @rdname mc_model
#' @export
mc_model_dots = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dots(...)

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

#' @rdname mc_model
#' @export
mc_model_dotsinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_dotsinterval(...)

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

#' @rdname mc_model
#' @export
mc_model_eye = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_eye(...)

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

#' @rdname mc_model
#' @export
mc_model_halfeye = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_halfeye(...)

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

#' @rdname mc_model
#' @export
mc_model_slab = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_slab(...)

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

#' @rdname mc_model
#' @export
mc_model_gradient = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_gradient(...)

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

#' @rdname mc_model
#' @export
mc_model_histinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_his(...)

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

#' @rdname mc_model
#' @export
mc_model_pointinterval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_pointinterval(...)

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

#' @rdname mc_model
#' @export
mc_model_interval = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_interval(...)

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

#' @rdname mc_model
#' @export
mc_model_lineribbon = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_lineribbon(...)

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

#' @rdname mc_model
#' @export
mc_model_ribbon = function(...) {
  p = function(mc_setting = NULL) {
    uncert_rep = uncertainty_rep_ribbon(...)

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

#' @rdname mc_model
#' @export
mc_model_geom = function(..., base_plot = "auto") {
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

#' @rdname mc_model
#' @export
mc_model_geom_auto = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = auto_plot(...)

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

#' @rdname mc_model
#' @export
mc_model_geom_point = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = point_plot(...)

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

#' @rdname mc_model
#' @export
mc_model_geom_line = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = line_plot(...)

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

#' @rdname mc_model
#' @export
mc_model_geom_tile = function(...) {
  p = function(mc_setting = NULL) {

    uncert_rep = tile_plot(...)

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

#' @rdname mc_model
#' @export
mc_model_geom_custom = function(plot, ...) {
  p = function(mc_setting = NULL) {

    uncert_rep = customized_plot(plot, ...)

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

