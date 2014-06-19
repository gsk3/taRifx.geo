\name{pointgrid2SpatialPolygons}
\alias{pointgrid2SpatialPolygons}
\title{Take a grid of regularly spaced points (such as those output by the centroids
of Arc's fishnet function) and convert it to various grid data types}
\usage{
pointgrid2SpatialPolygons(df, type)
}
\arguments{
  \item{df}{SpatialPointsDataFrame}

  \item{type}{"SpatialGrid" or "SpatialPolygons" or
  "SpatialPolygonsDataFrame"}
}
\value{
SpatialGrid, SpatialPolygons, SpatialPolygonsDataFrame
}
\description{
Take a grid of regularly spaced points (such as those
output by the centroids of Arc's fishnet function) and
convert it to various grid data types
}

