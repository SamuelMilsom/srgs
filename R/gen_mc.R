#' Creates a multiple choice style question
#'
#' @param p The number of possible answers for the question
#' @param c The number of correct answers for the question
#' @param m The score to be assigned if correct
#' @param l The score to be deducted if wrong
#' @param forall The logical condition describing whether the score is assigned if all the correct answers are chosen, or if it is assigned for each correct answer chosen
#' @return An object containing all the parameters of the question
#' @examples
#' question_1 <- gen_mc(p = 4, m = 3)
#' question_2 <- gen_mc(p = 6, c = 2, l = 1, forall = FALSE)

#' @export

gen_mc <- function(p, c = 1, m = 1, l = 0, forall = TRUE){
  if (is.logical(forall) == FALSE){
    stop("incorrect argument for 'forall'")
  }
  if (p <= 0){
    stop("p must be a positive integer")
  }
  if (c <= 0){
    stop("c must be a positive integer")
  }
  if (p%%1 != 0){
    stop("p must be a positive integer")
  }
  if (c%%1 != 0){
    stop("c must be a positive integer")
  }
  if (p <= c){
    stop("p must be greater than c")
  }
  if (m <= 0){
    stop("m must be a positive value")
  }
  if (l < 0){
    stop("input the loss mark as a positive value")
  }
  if (forall == TRUE){
    maxmark <- m
  } else if (forall == FALSE) {
    maxmark <- m*c
  }
  par_list <- list(p, c, m, forall, l, maxmark)
  class(par_list) <- c("mc", "question")
  return(par_list)
}
