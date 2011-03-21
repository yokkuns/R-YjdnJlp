\name{YjdnJlpDA-class}
\docType{class}
\alias{YjdnJlpDA-class}
\alias{toDataFrame,YjdnJlpDA-method}
\title{YjdnJlpDA Class}
\description{Result of DA}
\section{Objects from the Class}{
  \code{new('YjdnJlpDA',
    sentence = [character],
    chunk.list = [list])}
}

\section{Slots}{
	 \describe{
    \item{\code{sentence}:}{Japanese sentence}
    \item{\code{chunk.list}:}{Chunk object list}
  }
}
\section{Methods}{
  \describe{
    \item{toDataFrame}{Returns the DataFrame}
  }
}
\author{Yohei Sato}
\seealso{
  \code{\link{DAService}}
}
\keyword{classes}