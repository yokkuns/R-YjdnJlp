\name{YjdnJlpKeyphrase-class}
\docType{class}
\alias{YjdnJlpKeyphrase-class}
\alias{toDataFrame,YjdnJlpKeyphrase-method}
\title{YjdnJlpKeyphrase Class}
\description{Result of Keyphrase}
\section{Objects from the Class}{
  \code{new('YjdnJlpKeyphrase',
    sentence = [character],
    keyphrase.list = [list])}
}

\section{Slots}{
	 \describe{
    \item{\code{sentence}:}{Japanese sentence}
    \item{\code{keyphrase.list}:}{Keyphrase object list}
  }
}
\section{Methods}{
  \describe{
    \item{toDataFrame}{Returns the DataFrame}
  }
}
\author{Yohei Sato}
\seealso{
  \code{\link{Keyphrase}}
}
\keyword{classes}