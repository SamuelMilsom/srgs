context("summary pass percentage")
library(srgs)

test_that("summary pass percentage", {
  d_0 <- gen_mc(4)
  d_1 <- rep(d_0, 2)
  d_2 <- sim_dist(d_1, n = 100)
  expect_error(summary(d_2, -1), "pass percentage must be between 0 and 1")
  expect_error(summary(d_2, 2), "pass percentage must be between 0 and 1")
})
