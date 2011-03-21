\name{YjdnJlp-package}
\alias{YjdnJlp-package}
\alias{YjdnJlp}
\docType{package}
\title{
Japanese Text Analysis by Yahoo! Japan Develper Network
}
\description{
Japanese Text Analysis using Yahoo! Japan develper Network (http://developer.yahoo.co.jp/webapi/jlp/)
}
\details{
\tabular{ll}{
Package: \tab YjdnJlp\cr
Type: \tab Package\cr
Version: \tab 0.9.7\cr
Date: \tab 2011-03-20\cr
License: \tab BSD\cr
LazyLoad: \tab yes\cr
}

}
\author{
Yohei Sato <yokkun@tkul.jp>
}
\references{

}

\keyword{ package }
\seealso{
}
\examples{
   \dontrun{
       con <- initYjdnJlp()

       # MA
       ma <- MAService(con, "{japanese sentence}")
       ma.df <- toDataFrame(ma)
       head(ma.df)

       # DA
       da <- DAService(con, "{japanese sentence}")
       da.df <- toDataFrame(da)
       head(da.df)

       # Keyphrase
       keyphrase <- Keyprase(con, "{japanese sentence}")
       keyphrase.df <- toDataFrame(keyphrase)
       head(keyphrase.df)
    }
}
