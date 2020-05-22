#' A summary of results from a distribution
#'
#' @param object A simulated distribution
#' @param pass The percentage required in order to pass. Must be between 0 and 1.
#' @param probs A logic statement determining if the simulated probabilities should be printed.
#' @param ... Additional arguments affecting the summary produced.
#' @return Displays a variety of statistics from the simulated distribution including mean, median,
#' highest simulated mark, highest possible mark, the estimated probabilities of attaining each score,
#' the probability of passing based on estimated probabilities and a graphical representation of the
#' simulated distribution.
#' @examples
#' question_1 <- gen_mc(2)
#' test_1 <- rep(question_1, 5)
#' distribution_1 <- sim_dist(test_1)
#' summary(distribution_1)
#' summary(distribution_1, 0.6)

#' @importFrom graphics lines
#' @importFrom stats median

#' @export
summary.dist <- function(object, pass = 0.4, probs = TRUE, ...){
  if (pass < 0 | pass > 1){
    stop("pass percentage must be between 0 and 1")
  }
  a <- unlist(object[1])
  mm <- unlist(object[2])
  y <- c(a)
  cat("mean score is", mean(a), "\n")
  cat("median score is", median(a), "\n")
  cat("highest mark is", max(a), "\n")
  cat("the maxmimum mark for this test is", mm, "\n")
  a_prop_mat <- matrix(c(c(0:mm), rep(0, (2*(mm+1)))), nrow = (mm+1), byrow = FALSE)
  a_prop <- data.frame(a_prop_mat)
  colnames(a_prop) <- c("score", "frequency", "probability")
  for (i in 1:mm){
    a_prop$frequency[i] <- sum(a == (i-1))
  }
  a_prop$probability <- a_prop$frequency/length(a)
  colnames(a_prop) <- c("Score", "Freq", "Estimated Probability")
  if (probs == TRUE) {
    cat("estimated probabilities of this sample of size n =", length(a), "are", "\n")
    cat("", "\n")
    print(a_prop[, c("Score", "Estimated Probability")], row.names = FALSE, col.names = FALSE)
    cat("", "\n")
  }
  colnames(a_prop) <- c("score", "Freq", "probability")
  pass_m <- mm*pass
  a_prop$probability[(pass_m+1):(mm+1)]
  prob_pass <- sum(a_prop$probability[(pass_m+1):(mm+1)])
  cat("probability of passing with a pass percentage of", "%" ,100*pass, "by random guessing is", prob_pass, "\n")
  cat("for more accurate probabilities, increase n", "\n")
  plot(a_prop$score, a_prop$probability, xlab = "Score", ylab = "Probability", type = "n", xlim = c(0, mm))
  lines(a_prop$score, a_prop$probability)
}
