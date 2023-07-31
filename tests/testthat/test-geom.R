test_that("vanilla geom plot", {
  set.seed(1234)
  model_df = mtcars %>%
    dplyr::mutate(.row = row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.draw = list(1:10),
                  .prediction = list(rnorm(10, mpg, 5)),
                  mu = list(rnorm(10, mpg, 2)),
                  sigma = list(runif(10, 3, 6))) %>%
    tidyr::unnest(cols = c(.draw, .prediction, mu, sigma))

  vdiffr::expect_doppelganger("vanilla auto density plot",
                              model_df %>% mcplot())

  vdiffr::expect_doppelganger("vanilla auto point plot",
                              model_df %>% mcplot() + mc_condition_on(x = vars(disp)))

  vdiffr::expect_doppelganger("vanilla density plot",
                              model_df %>% mcplot() +
                                mc_model_line(stat = "density") +
                                mc_obs_line(stat = "density")
                              )

  vdiffr::expect_doppelganger("vanilla line plot",
                              model_df %>% mcplot() +
                                mc_model_line() +
                                mc_obs_line() +
                                mc_condition_on(x = vars(disp))
  )

  vdiffr::expect_doppelganger("vanilla point plot",
                              model_df %>% mcplot() +
                                mc_model_point() +
                                mc_obs_point() +
                                mc_condition_on(x = vars(disp))
  )
})
