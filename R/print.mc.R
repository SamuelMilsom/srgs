#' @export
print.mc <- function(x, ...){
  cat("multiple choice question", "\n")
  cat("number of possible answers is", unlist(x[1]), "\n")
  cat("number of correct answers is", unlist(x[2]), "\n")
  cat("score per correct answer is", unlist(x[3]), "\n")
  if (unlist(x[4]) == TRUE){
    cat("score only awarded if all are correctly chosen", "\n")
  } else if (unlist(x[4]) == FALSE) {
    cat("score awarded for each correct choice", "\n")
  }
  cat("maximum possible mark is", unlist(x[6]), "\n")
  cat("loss mark is", unlist(x[5]), "\n")
  cat("recommended loss mark is", mark_loss(x), "\n")
}
