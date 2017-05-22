\name{BinaryMatrix}
\alias{BinaryMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BinaryMatrix
}
\description{
BinaryMatrix takes the file 'Good_Predictors' created by Predictive_Power function and creates a list of files containing the final signature, the two lists of peculiar genes for the two classes.
}
\usage{
BinaryMatrix(binary_matrix_file, thr_low, thr_high, classification_vector_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{binary_matrix_file}{ the 'Good_Predictors' file created by Predictive_Power function
}
 \item{thr_low}{The threshold identifying the most misclassified subjects in class 0}
 \item{thr_high}{The threshold identifying the most misclassified subjects in class 1}
 \item{classification_vector_file}{A file containing the true classification labels of the samples}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
BinaryMatrix creates three files:
- 'Final_Signature' containing all the peculiar genes
- 'Signature_High' containing the list of peculiar genes for class 1
- 'Signature_Low' containing the list of peculiar genes for class 0.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Federica Martina
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as
BinaryMatrix('Good_Predictors', 20, 165, 'classification_vector')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
