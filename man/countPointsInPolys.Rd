\name{countPointsInPolys}
\alias{countPointsInPolys}
\title{Count points within a polygon}
\usage{
  countPointsInPolys(points, polys, density = FALSE)
}
\arguments{
  \item{points}{SpatialPoints}

  \item{polys}{SpatialPolygonsDataFrame}

  \item{density}{Return a density (point count divided by
  area) instead of a point count}
}
\value{
  SpatialPolygonsDataFrame
}
\description{
  Overlays points on polygons and create a new polygon
  dataset with the count of the points in that polygon
}
\seealso{
  See Also as \code{\link[sp]{overlay}}
}

