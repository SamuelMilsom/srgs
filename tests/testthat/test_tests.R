context("tests class")
library(srgs)

test_that("tests are the correct class", {
  q_1 <- gen_mc(4)
  q_2 <- gen_match(4)
  test_1 <- rep(q_1, 2)
  test_2 <- compile(q_1, q_2)
  test_3 <- compile(test_1, test_2)
  expect_is(test_1, "test")
  expect_is(test_2, "test")
  expect_is(test_3, "test")
})

