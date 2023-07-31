test_that("vanilla comparative layouts", {
  set.seed(1234)
  model_df = mtcars %>%
    dplyr::mutate(.row = row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.draw = list(1:10),
                  .prediction = list(rnorm(10, mpg, 5)),
                  mu = list(rnorm(10, mpg, 2)),
                  sigma = list(runif(10, 3, 6))) %>%
    tidyr::unnest(cols = c(.draw, .prediction, mu, sigma))

  vdiffr::expect_doppelganger("vanilla superposition",
                              model_df %>% mcplot() +
                                mc_model_ccdf() +
                                mc_obs_ccdf() +
                                mc_condition_on(x = vars(cyl)) +
                                mc_layout_superposition())

  vdiffr::expect_doppelganger("vanilla juxtaposition",
                              model_df %>% mcplot() +
                                mc_model_ccdf() +
                                mc_obs_ccdf() +
                                mc_condition_on(x = vars(cyl)) +
                                mc_layout_juxtaposition())

  vdiffr::expect_doppelganger("vanilla nested juxtaposition",
                              model_df %>% mcplot() +
                                mc_model_ccdf() +
                                mc_obs_ccdf() +
                                mc_condition_on(x = vars(cyl)) +
                                mc_layout_nested())

  vdiffr::expect_doppelganger("vanilla residual plot",
                              model_df %>% mcplot() +
                                mc_model_dots() +
                                mc_obs_dots() +
                                mc_condition_on(x = vars(cyl)) +
                                mc_layout_encoding(transform = "residual"))
})
