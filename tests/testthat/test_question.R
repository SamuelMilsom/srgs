context("Parameter values and class of questions")
library(srgs)

test_that("parameters are positive", {
  expect_error(gen_mc(p = -2), "p must be a positive integer")
  expect_error(gen_match(p = -2), "p must be a positive integer")
  expect_error(gen_mc(p = 4, c = -1), "c must be a positive integer")
  expect_error(gen_mc(4, m = -2), "m must be a positive value")
  expect_error(gen_match(4, m = -2), "m must be a positive value")
  expect_error(gen_mc(4, l = -2), "input the loss mark as a positive value")
  expect_error(gen_match(4, l = -2), "input the loss mark as a positive value")
})
test_that("parameters are integers", {
  expect_error(gen_mc(p = 2.5), "p must be a positive integer")
  expect_error(gen_match(p = 2.5), "p must be a positive integer")
  expect_error(gen_mc(p = 4, c = 1.5), "c must be a positive integer")
})
test_that("p is smaller than c", {
  expect_error(gen_mc(p = 4, c = 5), "p must be greater than c")
})
test_that("forall is a logical condition", {
  expect_error(gen_mc(p = 4, forall = 3), "incorrect argument for 'forall'")
  expect_error(gen_match(p = 4, forall = 3), "incorrect argument for 'forall'")
})
test_that("questions have the correct class", {
  expect_is(gen_mc(4), "mc")
  expect_is(gen_mc(4), "question")
  expect_is(gen_match(4), "match")
  expect_is(gen_match(4), "question")
})
