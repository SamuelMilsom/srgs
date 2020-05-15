#' Find the maximum mark
#'
#' @param x A test
#' @return The maximum possible mark for that test.
#' @examples
#' question_1 <- gen_mc(2)
#' test_1 <- compile(question_1)
#' max_mark(test_1)

#' @importFrom utils tail

#' @export
max_mark <- function(x){
  UseMethod("max_mark")
}
#' @export
max_mark.default <- function(x){
  stop("incorrect object")
}
#' @export
max_mark.test <- function(x){
  mm <- 0
  p <- length(x)
  for (i in 1:p){
    a <- unlist(unlist(x[i]))
    b <- tail(a, n = 1)
    mm <- mm + b
  }
  mm
  maxmark <- as.numeric(mm)
  return(maxmark)
}
