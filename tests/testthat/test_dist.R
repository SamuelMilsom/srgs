context("distributions class and parameters")
library(srgs)

test_that("distributions are the correct class", {
  d_0 <- gen_mc(4)
  d_1 <- rep(d_0, 2)
  d_2 <- sim_dist(d_1, n = 100)
  expect_is(d_2, "dist")
})
test_that("distributions parameters are positive integers", {
  expect_error(sim_dist(rep(gen_mc(4), 2), n = -100), "n must be a positive integer")
  expect_error(sim_dist(rep(gen_mc(4), 2), n = 100.8), "n must be a positive integer")
})
