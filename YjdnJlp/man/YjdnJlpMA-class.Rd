\name{YjdnJlpMA-class}
\docType{class}
\alias{YjdnJlpMA-class}
\alias{toDataFrame,YjdnJlpMA-method}
\title{YjdnJlpMA Class}
\description{Result of MA}
\section{Objects from the Class}{
  \code{new('YjdnJlpMA',
    sentence = [character],
    total.count = [numeric],
    filtered.count = [numeric],
    word.list = [list])}
}

\section{Slots}{
	 \describe{
    \item{\code{sentence}:}{Japanese sentence}
    \item{\code{total.count}:}{total morpehm count}
    \item{\code{filtered.count}:}{uniq morphem count}
    \item{\code{word.list}:}{Word object list}
  }
}
\section{Methods}{
  \describe{
    \item{toDataFrame}{Returns the DataFrame}
  }
}
\author{Yohei Sato}
\seealso{
  \code{\link{MAService}}
}
\keyword{classes}