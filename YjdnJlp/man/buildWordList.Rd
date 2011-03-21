\name{buildWordList}
\alias{buildWordList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
build Word list
}
\description{
build Word list
}
\usage{
buildWordList(raw.word.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw.word.list}{
%%     ~~Describe \code{raw.word.list} here~~
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
function(raw.word.list)
{
    word.list <- lapply(raw.word.list, function(x){ buildWord(x$surface,x$reading,x$pos,x$baseform, x$count) } )
    names(word.list) <- NULL
    word.list
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
