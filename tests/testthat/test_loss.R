context("loss mark parameters")
library(srgs)

test_that("loss marks are positive values", {
  t_1 <- rep(gen_mc(4), 2)
  expect_error(set_loss(t_1, -1), "input the loss mark as a positive value")
})
