#' Simulate a random guess at a question
#'
#' @param x A question
#' @return A score produced by simulating a random guess at a question under its defined parameters.
#' @examples
#' question_1 <- gen_mc(2)
#' question_2 <- gen_match(4)
#' qsim(question_1)
#' qsim(question_2)

#' @importFrom stats runif

#' @export
qsim <- function(x){
  UseMethod("qsim")
}
#' @export
qsim.default <- function(x){
  stop("incorrect object")
}
#' @export
qsim.match <- function(x){
  s <- 0
  p <- unlist(x[1])
  m <- unlist(x[2])
  forall <- unlist(x[3])
  l <- unlist(x[4])
  if (forall == TRUE){
    t <- 1/(factorial(p))
    u <- runif(1, min = 0, max = 1)
    if (u < t){
      s <- s + m
    } else {
      s <- s - l
    }
  } else if (forall == FALSE){
    perm <- sample(1:p)
    check <- perm == (1:p)
    result <- sum(check)
    s <- s + (m*result) - (l*(p-result))
  } else {
    stop("incorrect argument for forall")
  }
  if (s < 0){
    s <- 0
  }
  s
  return(s)
}
#' @export
qsim.mc <- function(x){
  s <- 0
  p <- unlist(x[1])
  c <- unlist(x[2])
  m <- unlist(x[3])
  forall <- unlist(x[4])
  l <- unlist(x[5])
  if (forall == TRUE){
    t <- 1/choose(p, c)
    u <- runif(1, min = 0, max = 1)
    if (u < t){
      s <- s + m
    } else {
      s <- s - l
    }
  } else if (forall == FALSE) {
    poss <-  c(1:p)
    corr <- c(1:c)
    perm <- sample(poss, c)
    check <- length(intersect(perm, corr))
    s <- s + (m*check) - (l*(c-check))
  }
  if (s < 0){
    s <- 0
  }
  s
  return(s)
}
