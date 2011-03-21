\name{DAService}
\alias{DAService}
\title{
Japanese Dependency Analysis
}
\description{
Japanese Dependency Analysis by Yahoo! Japan Developer Network.
}
\usage{
DAService(object, sentence)
}

\arguments{
  \item{object}{
YjdnJlp object
}
  \item{sentence}{
Japanese sentence
}
}
\details{

}
\value{
YjdnJlpDA object
}
\references{
http://developer.yahoo.co.jp/webapi/jlp/da/v1/parse.html
}
\author{
Yohei Sato
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(object, sentence)
    standardGeneric("DAService")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ method }
