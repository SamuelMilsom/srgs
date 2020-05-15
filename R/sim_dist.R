#' Simulate the distribution of random guess scores for a test
#'
#' @param x A test
#' @param n The number of simulations to be made. Increasing n will increase the accuracy of the simulation.
#' @return An object containing the n results of the simulation and the maximum possible mark for the test. All values are rounded accordingly.
#' @examples
#' question_1 <- gen_mc(2)
#' test_1 <- rep(question_1, 5)
#' distribution_1 <- sim_dist(test_1)
#' distribution_2 <- sim_dist(test_1, n = 500)

#' @export
sim_dist <- function(x, n = 10000){
  UseMethod("sim_dist")
}
#' @export
sim_dist.default <- function(x, n = 10000){
  stop("incorrect object")
}
#' @export
sim_dist.test <- function(x, n = 10000){
  if (n <= 0){
    stop("n must be a positive integer")
  }
  if (n%%1 != 0){
    stop("n must be a positive integer")
  }
  score <- rep(0, n)
  for (i in 1:n){
    score[i] <- sum(sapply(x, qsim))
    score[i] <- round(score[i])
  }
  score
  maxmark <- max_mark(x)
  score <- list(score, maxmark)
  class(score) <- "dist"
  return(score)
}
