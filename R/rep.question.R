#' Repeat questions into a test
#'
#' @param x The question to be repeated.
#' @param n The number of times to be repeated.
#' @param ... Further arguments to be passed to or from other methods.
#' @return A test consisting of the given test n times.
#' @examples
#' question_1 <- gen_mc(2)
#' test_1 <- rep(question_1, 10)
#' test_2 <- rep(question_1, 5)

#' @export
rep.question <- function(x, n, ...){
  test <- rep(list(x), n)
  class(test) <- "test"
  return(test)
}
