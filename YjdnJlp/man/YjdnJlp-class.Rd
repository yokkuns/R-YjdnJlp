\name{YjdnJlp-class}
\docType{class}
\alias{YjdnJlp-class}
\alias{MAService,YjdnJlp-method}
\alias{DAService,YjdnJlp-method}
\alias{Keyphrase,YjdnJlp-method}
\alias{post,YjdnJlp-method}
\title{YjdnJlp Class}
\description{connection of Yahoo! Japan Developer Network}
\section{Objects from the Class}{
  \code{new('YjdnJlp',
    appid = [character],
    handle = [CURLHandle],
    ma.service = [character],
    da.service = [character],
    keyphrase = [character])}
}

\section{Slots}{
	 \describe{
    \item{\code{appid}:}{Appication ID of Yahoo! Japan Develper Network }
    \item{\code{handle}:}{CURL Handle }
    \item{\code{ma.service}:}{MA Service URI }
    \item{\code{da.service}:}{DA Service URI }
    \item{\code{keyphrase}:}{Keyphrase Service URI }
  }
}
\section{Methods}{
  \describe{
    \item{post}{Request post method}
    \item{MAService}{Returns the YjdnJlpMA object}
    \item{DAService}{Returns the YjdnJlpDA object}
    \item{Keyphrase}{Returns the YjdnJlpKeyhprase objecyt}
  }
}
\author{Yohei Sato}
\seealso{
  \code{\link{initYjdnJlp}}
}
\keyword{classes}
