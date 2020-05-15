#' @export
print.match <- function(x, ...){
  cat("matching question", "\n")
  cat("number of answers to be matched is", unlist(x[1]), "\n")
  cat("score per correct answer is", unlist(x[2]), "\n")
  if (unlist(x[3]) == TRUE){
    cat("score only awarded if all are correctly matched", "\n")
  } else if (unlist(x[3]) == FALSE) {
    cat("score awarded for each correct match", "\n")
  }
  cat("maximum possible mark is", unlist(x[5]), "\n")
  cat("loss mark is", unlist(x[4]), "\n")
  cat("recommended loss mark is", mark_loss(x), "\n")
}
