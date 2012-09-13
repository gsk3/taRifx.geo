\name{reshapeSLDF}
\alias{reshapeSLDF}
\title{Reshape a spatialLinesDataFrame into a series of points with associated
information}
\usage{
  reshapeSLDF(SLDF, shape = "long")
}
\arguments{
  \item{SLDF}{spatialLinesDataFrame}

  \item{shape}{Do not change.  Must be "long".  For forward
  compatility.}
}
\value{
  data.frame
}
\description{
  Reshape a spatialLinesDataFrame into a series of points
  with associated information (less efficient because all
  the segment data gets replicated over each point)
}
\seealso{
  See Also \code{\link{SLDFtoLine}}
}

