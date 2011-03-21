\name{buildChunk}
\alias{buildChunk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
build chunk object
}
\description{
build chunk object
}
\usage{
buildChunk(id, dependency, morphem.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{dependency}{
%%     ~~Describe \code{dependency} here~~
}
  \item{morphem.list}{
%%     ~~Describe \code{morphem.list} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Yohei Sato
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(id, dependency, morphem.list)
{
    new("Chunk",
        id=id,
        dependency=dependency,
        morphem.list=morphem.list
       )
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
