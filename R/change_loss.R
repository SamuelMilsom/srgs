#' Changes the loss mark of a question
#'
#' @param x A question
#' @param a The new loss mark
#' @return A question with the same parameters but with new loss mark
#' @examples
#' question_1 <- gen_mc(2)
#' question_1 <- change_loss(question_1, 1)
#' question_2 <- change_loss(question_1, 0)

#' @export
change_loss <- function(x, a){
  UseMethod("change_loss")
}
#' @export
change_loss.default <- function(x, a){
  stop("incorrect object")
}
#' @export
change_loss.match <- function(x, a){
  x[[4]] <- a
  return(x)
}
#' @export
change_loss.mc <- function(x, a){
  x[[5]] <- a
  return(x)
}
