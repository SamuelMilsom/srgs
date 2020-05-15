#' Find the optimum loss mark
#'
#' @param x A question or a test
#' @return For a question, `mark_loss` will output the mark to be deducted
#' if answered incorrectly such that the expected mark from random guessing
#' is negative. For a test, it outputs the average loss mark.
#' @examples
#' question_1 <- gen_mc(2)
#' test_1 <- compile(question_1, question_1)
#' mark_loss(question_1)
#' mark_loss(test_1)

#' @export
mark_loss <- function(x){
  UseMethod("mark_loss")
}
#' @export
mark_loss.default <- function(x){
  stop("incorrect object")
}
#' @export
mark_loss.match <- function(x){
  p <- unlist(x[1])
  m <- unlist(x[2])
  forall <- unlist(x[3])
  if (forall == TRUE){
    prob_correct <- 1/factorial(p)
    prob_wrong <- 1-prob_correct
    l <- (m*prob_correct)/prob_wrong
  } else if (forall == FALSE){
    a <- c(rep(0, p+1))
    b_1 <- rep(0, p+1)
    b_2 <- rep(0, p+1)
    dera <- function(x){
      y <- ((-1)^x)/factorial(x)
      y
      return(y)
    }
    for (k in 0:p){
      a_a <- c(seq(0, p-k))
      a_b <- sapply(a_a, dera)
      a_c <- sum(a_b)
      a[k+1] <- (1/factorial(k))*a_c
    }
    for (j in 1:(p+1)){
      k <- j-1
      b_1[j] <- a[j]*k*m
      b_2[j] <- a[j]*(p-k)
    }
    l <- sum(b_1)/sum(b_2)
  }
  l
  return(l)
}
#' @export
mark_loss.mc <- function(x){
  p <- unlist(x[1])
  c <- unlist(x[2])
  m <- unlist(x[3])
  forall <- unlist(x[4])
  if (forall == TRUE){
    prob_correct <- 1/choose(p, c)
    prob_wrong <- 1-prob_correct
    l <- (m*prob_correct)/prob_wrong
  } else if (forall == FALSE){
    a <- rep(0, c+1)
    b_1 <- rep(0, c+1)
    b_2 <- rep(0, c+1)
    for (i in 1:(c+1)){
      k <- i-1
      if ((p-(2*c)+k) >= 0){
        a_1 <- factorial(c)/factorial(c-k)
        a_2 <- factorial(p-k)/factorial(p)
        a_3 <- factorial(p-c)/factorial(p-(2*c)+k)
        a_4 <- factorial(p-c)/factorial(p-k)
        a[i] <- choose(c, k)*a_1*a_2*a_3*a_4
      } else {
        a[i] <- 0
      }
    }
    for (j in 1:(c+1)){
      k <- j-1
      b_1[j] <- a[j]*k*m
      b_2[j] <- a[j]*(c-k)
    }
    l <- sum(b_1)/sum(b_2)
  }
  l
  return(l)
}
#' @export
mark_loss.test <- function(x){
  all_poss <- c(sapply(x, mark_loss))
  rec <- mean(all_poss)
  return(rec)
}
