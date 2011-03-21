\name{buildChunkList}
\alias{buildChunkList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
build chunk list
}
\description{
build chunk list
}
\usage{
buildChunkList(raw.chunk.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw.chunk.list}{
%%     ~~Describe \code{raw.chunk.list} here~~
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
function(raw.chunk.list)
{
    chunk.list <- list()

    for( node in raw.chunk.list ){
        morphem.list <- lapply(node$MorphemList, function(x){ buildMorphem(x$Surface,x$Reading,x$Baseform,x$POS,x$Feature)})
        id <- as.numeric(node$Id) + 1
        dependency <- as.numeric(node$Dependency) + 1
        dependency <- ifelse(dependency==0, -1, dependency)
        chunk.list[[id]] <- buildChunk(id, dependency, morphem.list)
    }

    chunk.list
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
