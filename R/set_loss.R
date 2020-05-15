#' Change the loss mark for every question in a test
#'
#' @param x A test
#' @param a The new loss mark
#' @return An identical test but with every question having the new loss mark parameter.
#' @examples
#' question_1 <- gen_mc(2)
#' test_1 <- compile(question_1)
#' test_1 <- set_loss(test_1, 0.5)
#' test_2 <- set_loss(test_1, 0)

#' @export
set_loss <- function(x, a){
  UseMethod("set_loss")
}
#' @export
set_loss.default <- function(x, a){
  stop("incorrect object")
}
#' @export
set_loss.test <- function(x, a){
  if (a < 0){
    stop("input the loss mark as a positive value")
  }
  for (i in 1:length(x)){
    x[[i]] <- change_loss(x[[i]], a)
  }
  return(x)
}
