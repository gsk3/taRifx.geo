\name{SLDFtoLine}
\alias{SLDFtoLine}
\title{Convert a SpatialLinesDataFrame to a single line matrix with associated segment information}
\usage{
SLDFtoLine(lineDF, orderXY = FALSE, segments = TRUE)
}
\arguments{
  \item{lineDF}{SpatialLinesDataFrame object}

  \item{orderXY}{ordering}

  \item{segments}{segments to associate}
}
\value{
data.frame
}
\description{
Convert a SpatialLinesDataFrame to a single line matrix
with associated segment information
}
\seealso{
See Also \code{\link{reshapeSLDF}}
}

