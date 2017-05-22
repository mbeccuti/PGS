\name{DEG_detection}
\alias{DEG_detection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DEG_detection
}
\description{
DEG_detection uses 'limma' package to detect differentially expressed genes.
It creates a matrix containing all the DEG detected in the gene expression data set in input using the contrast file.
}
\usage{
DEG_detection(X,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  Gene expression matrix
  }
  \item{y}{
  contrast file
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
DEG_detection returns DEGs_file, containing the gene expression of the DEG detected by 'limma' package.

The function also displays the number of DEGs detected.

}
\references{
Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W and Smyth GK (2015). “limma powers differential expression analyses for RNA-sequencing and microarray studies.” Nucleic Acids Research, 43(7), pp. e47.
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
DEG_detection('Matrix_Genes', 'contrast_file')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
