\name{countPointsInPolys}
\alias{countPointsInPolys}
\title{Count points within a polygon}
\usage{
  countPointsInPolys(points, polys, density = FALSE,
    by = NULL)
}
\arguments{
  \item{points}{SpatialPoints}

  \item{polys}{SpatialPolygonsDataFrame}

  \item{density}{Return a density (point count divided by
  area) instead of a point count}

  \item{by}{Factor to return counts by.  For instance, if
  by is a factor with two levels, instead of a single count
  variable being returned, two variables will be returned--
  the count of point type A in the polygon, and the count
  of point type B.  The by factor must be of length
  length(points).}
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

