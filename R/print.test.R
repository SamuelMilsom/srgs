#' @export
print.test <- function(x, ...){
  mc <- 0
  mat <- 0
  a <- length(x)
  for (i in 1:a){
    if (class(x[[i]])[1] == "mc"){
      mc <- mc + 1
    }
    if (class(x[[i]])[1] == "match"){
      mat <- mat + 1
    }
  }
  cat("a test consisting of", mc, "multiple choice questions and", mat, "matching questions.", "\n")
  cat("the maximum mark for this test is", max_mark(x), "\n")
  cat("the recommended loss mark for this test is", mark_loss(x), "\n")
}
