% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rep.question.R
\name{rep.question}
\alias{rep.question}
\title{Repeat questions into a test}
\usage{
\method{rep}{question}(x, n, ...)
}
\arguments{
\item{x}{The question to be repeated.}

\item{n}{The number of times to be repeated.}

\item{...}{Further arguments to be passed to or from other methods.}
}
\value{
A test consisting of the given test n times.
}
\description{
Repeat questions into a test
}
\examples{
question_1 <- gen_mc(2)
test_1 <- rep(question_1, 10)
test_2 <- rep(question_1, 5)
}
