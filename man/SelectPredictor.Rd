\name{SelectPredictor}
\alias{SelectPredictor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SelectPredictor
}
\description{
SelectPredictors takes a gene expression matrix with two values for each samples and compute the fold-change, defined as the difference in the gene expression levels between the two condition tested in the experiment. It returns a file containing only one value for each sample.
}
\usage{
SelectPredictor(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A gene expression matrix where each sample has two paired columns containing the gene expression levels of the DEGs detected using DEG_detection function in the two different biological condition tested.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
SelectPredictor creates a file containing a gene expression matrix where each column contains the difference in the gene expression levels for each sample.
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
SelectPredictor('DEGs_file')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
