\name{Predictive_Power}
\alias{Predictive_Power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Predictive_Power
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Predictive_Power is used to fit logistic regression model using DEG gene expression to predict the class labels of the samples. It creates two outputs: a distribution of predictive power associated to each DEG and a Binary Matrix of predicted class labels.
}
\usage{
Predictive_Power(X, y, intercept = TRUE, pp_thr, q_thr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{X}{Matrix containing predictors values.}
  \item{y}{True classification vector.}
  \item{intercept}{Logical. To include the intercept in the regression model. The default setting is TRUE.}
  \item{pp_thr}{The threshold to switch from the fitted probabilities to predicted class labels. The default value is 0.5}
  \item{q_thr}{The quantile of the Predictive Powers distribution to be chosen as 'Good Predictors'. The defaul setting is 0.95}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Predictive_Power returns two files: Predictors_pp and Good_Predictors.

Predictors_pp contains all the predictors names with the associated predictive power.
Good_Predictors is the binary matrix containing the class labels of the samples predicted by the DEGs belonging to the selected quantile (qq_thr) of the  Predictors_pp distribution.
}
\references{
  Liao J, Chin KV.
  Logistic regression for disease classification using microarray data:
    model selection in a large p and small n case.
  Bioinformatics. 2007;23(15):1945--1951.

  Van't Veer, Laura J., et al. "Gene expression profiling predicts clinical outcome of breast cancer." nature 415.6871 (2002): 530-536.
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

Predictive_Power('Matrix_Genes', 'classification_vector', intercept = T, pp_thr = 0.6, q_thr = .95)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
