\name{interpolatePolyPoint}
\alias{interpolatePolyPoint}
\title{Interpolate points from polygon SPDF
This function returns (weighted) sample points in fine for every polygon in crude.
Thus running it repeatedly gives you useful variation that reflects the interpolation uncertainty.}
\usage{
interpolatePolyPoint(crude, fine, weightCol = NULL, nSampleCol = 1,
  replace = TRUE, verbose = FALSE)
}
\arguments{
  \item{crude}{A SpatialPolygonsDataFrame.}

  \item{fine}{A SpatialPointsDataFrame.}

  \item{weightCol}{A column name in fine to weight the
  point sampling by, or NULL if no weighting is required}

  \item{nSampleCol}{Either a column name in crude
  containing number of elements of fineWithin to sample per
  polygon, or a number of points to sample per polygon}

  \item{replace}{A logical indicating whether to sample
  points from fine with replacement or not within each
  polygon of crude}

  \item{verbose}{Whether to output detailed error messages}
}
\value{
A SpatialPointsDataFrame containing
}
\description{
Interpolate points from polygon SPDF This function returns
(weighted) sample points in fine for every polygon in
crude. Thus running it repeatedly gives you useful
variation that reflects the interpolation uncertainty.
}
\examples{
\dontrun{
replicate( 10, interpolatePolyPoint(
  crude=polySP, fine=pointSP,
  weightCol="pop", nSampleCol="z",
  replace=TRUE, verbose=TRUE ),
simplify=FALSE )
}
}

