test_that("vanilla distribution", {
  set.seed(1234)
  model_df = mtcars %>%
    dplyr::mutate(.row = row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(.draw = list(1:10),
                  .prediction = list(rnorm(10, mpg, 5)),
                  mu = list(rnorm(10, mpg, 2)),
                  sigma = list(runif(10, 3, 6))) %>%
    tidyr::unnest(cols = c(.draw, .prediction, mu, sigma))

  vdiffr::expect_doppelganger("vanilla predictive distribution",
                              model_df %>% mcplot()
                              )

  vdiffr::expect_doppelganger("vanilla mu distribution",
                              model_df %>% mcplot() + mc_distribution("mu")
  )

  sd_function = function(df) {
    df %>% mutate(observation = sd(observation))
  }

  vdiffr::expect_doppelganger("vanilla sigma distribution",
                              model_df %>% mcplot(observation_transform = sd_function) +
                                mc_distribution("sigma") +
                                mc_condition_on(x = vars(disp))
  )
})