\name{buildWord}
\alias{buildWord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
build Word object
}
\description{
build Word object
}
\usage{
buildWord(surface, reading, pos, baseform, count)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{surface}{
%%     ~~Describe \code{surface} here~~
}
  \item{reading}{
%%     ~~Describe \code{reading} here~~
}
  \item{pos}{
%%     ~~Describe \code{pos} here~~
}
  \item{baseform}{
%%     ~~Describe \code{baseform} here~~
}
  \item{count}{
%%     ~~Describe \code{count} here~~
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
function(surface, reading, pos, baseform, count)
{
    new("Word",
        surface=as.character(surface),
        reading=ifelse(is.null(reading), "", reading),
        pos=as.character(pos),
        baseform=as.character(baseform),
        count=as.numeric(count))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
