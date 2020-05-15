#' Creates a matching style question
#'
#' @param p Number of matches to be made for the question
#' @param m The score to be assigned if correct
#' @param l The score to be deducted if wrong
#' @param forall The logical condition describing whether the score is assigned if all the matches are correct, or if it is assigned for each correct match
#' @return An object containing all the parameters of the question
#' @examples
#' question_1 <- gen_match(p = 5, m = 3)
#' question_2 <- gen_match(p = 6, l = 1, forall = FALSE)

#' @export

gen_match <- function(p, m = 1, l = 0, forall = TRUE){
  if (is.logical(forall) == FALSE){
    stop("incorrect argument for 'forall'")
  }
  if (p <= 0){
    stop("p must be a positive integer")
  }
  if (p%%1 != 0){
    stop("p must be a positive integer")
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
    maxmark <- m*p
  }
  par_list <- list(p, m, forall, l, maxmark)
  class(par_list) <- c("match", "question")
  return(par_list)
}
