#' Compile questions into a test
#'
#' @param ... A group of either questions or tests
#' @return A test comprimising of the given questions, or a new test comprimising of a combination of tests.
#' @examples
#' question_1 <- gen_mc(2)
#' question_2 <- gen_match(2)
#' test_1 <- compile(question_1, question_2)
#' test_2 <- compile(question_1, question_1)
#' test_3 <- compile(test_1, test_2)

#' @export
compile <- function(...){
  UseMethod("compile")
}
#' @export
compile.default <- function(...){
  stop("incorrect object(s)")
}
#' @export
compile.question <- function(...){
  test <- list(...)
  class(test) <- "test"
  return(test)
}
#' @export
compile.test <- function(...){
  test <- c(...)
  class(test) <- "test"
  return(test)
}
