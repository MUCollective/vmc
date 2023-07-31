test_that("vanilla certainty representations", {
  set.seed(1234)
  model_df = mtcars %>%
    dplyr::mutate(.row = row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.draw = list(1:10),
                  .prediction = list(rnorm(10, mpg, 5)),
                  mu = list(rnorm(10, mpg, 2)),
                  sigma = list(runif(10, 3, 6))) %>%
    tidyr::unnest(cols = c(.draw, .prediction, mu, sigma))

  vdiffr::expect_doppelganger("vanilla ccdf",
                              model_df %>% mcplot() +
                                mc_model_ccdf() +
                                mc_obs_ccdf() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla cdf",
                              model_df %>% mcplot() +
                                mc_model_cdf() +
                                mc_obs_cdf() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla dots",
                              model_df %>% mcplot() +
                                mc_model_dots() +
                                mc_obs_dots() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla dotsinterval",
                              model_df %>% mcplot() +
                                mc_model_dotsinterval() +
                                mc_obs_dotsinterval() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla eye",
                              model_df %>% mcplot() +
                                mc_model_eye() +
                                mc_obs_eye() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla gradient",
                              model_df %>% mcplot() +
                                mc_model_gradientinterval() +
                                mc_obs_gradientinterval() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla halfeye",
                              model_df %>% mcplot() +
                                mc_model_halfeye() +
                                mc_obs_halfeye() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla histogram",
                              model_df %>% mcplot() +
                                mc_model_histinterval() +
                                mc_obs_histinterval() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla interval",
                              model_df %>% mcplot() +
                                mc_model_interval() +
                                mc_obs_interval() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla lineribbon",
                              model_df %>% mcplot() +
                                mc_model_lineribbon() +
                                mc_obs_lineribbon() +
                                mc_condition_on(x = vars(disp)))

  vdiffr::expect_doppelganger("vanilla pointinterval",
                              model_df %>% mcplot() +
                                mc_model_pointinterval() +
                                mc_obs_pointinterval() +
                                mc_condition_on(x = vars(disp)))

  vdiffr::expect_doppelganger("vanilla ribbon",
                              model_df %>% mcplot() +
                                mc_model_ribbon() +
                                mc_obs_ribbon() +
                                mc_condition_on(x = vars(disp)))

  vdiffr::expect_doppelganger("vanilla slab",
                              model_df %>% mcplot() +
                                mc_model_slab() +
                                mc_obs_slab() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla slab grouping draws",
                              model_df %>% mcplot() +
                                mc_model_slab(draw = "group", alpha = .1) +
                                mc_obs_slab() +
                                mc_condition_on(x = vars(cyl)))

  vdiffr::expect_doppelganger("vanilla slab aggregating draws",
                              model_df %>% mcplot() +
                                mc_model_slab(draw = mean) +
                                mc_obs_slab() +
                                mc_condition_on(x = vars(cyl)))

})
