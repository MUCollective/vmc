
uncertainty_rep_dotsinterval = function(..., draw = "collapse") {
  function(samples, row_vars, col_vars, labels, axis_type, model_color, is_animation, y_var) {
    zeallot::`%<-%`(c(x_type, y_type), axis_type)
    y_axis_order = sort(unique(samples[[rlang::quo_name(y_var)]]))
    
    if (draw == "collapse") {
      return(c(ggdist::stat_dotsinterval(data = samples, 
                                 ggplot2::aes(y = !!y_var,
                                              color = model_color, fill = model_color), 
                                 ...
                                 )))
    } else if (draw == "group") {
      return(c(ggdist::stat_dotsinterval(data = samples, 
                                 ggplot2::aes(y = !!y_var,
                                              group = .draw,
                                              color = model_color, fill = model_color), 
                                 ...
                                 )))
    } else if (draw == "hops") {
      hops_id = get_unique_id()
      draw_col = paste(".draw", hops_id, sep = "")
      return(c(ggdist::stat_dotsinterval(data = samples |>
                                   dplyr::mutate(".draw{{hops_id}}" := .draw), 
                                 ggplot2::aes(y = !!y_var,
                                              color = model_color, fill = model_color), 
                                 ...
                                 ),
               gganimate::transition_manual(!!rlang::sym(draw_col), cumulative = FALSE)))
    } else if (is.function(draw)) {
      # if (is.null(agg_func)) {
      #   agg_func = mean
      # }
      
      return(c(ggdist::stat_dotsinterval(data = samples |> 
                                   dplyr::group_by_at(c(ggplot2::vars(.row, x_axis), row_vars, col_vars)) |>
                                   dplyr::summarise(y_agg = draw(!!y_var)), 
                                 ggplot2::aes(y = y_agg,
                                              color = model_color, fill = model_color), 
                                 ...
                                 )))
    }
  }
}