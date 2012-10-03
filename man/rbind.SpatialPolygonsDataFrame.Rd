\name{rbind.SpatialPolygonsDataFrame}
\alias{rbind.SpatialPolygonsDataFrame}
\title{rbind SpatialPolygonsDataFrames together, fixing IDs if duplicated}
\usage{
  \method{rbind.SpatialPolygonsDataFrame}{} (...,
    fix.duplicated.IDs = TRUE)
}
\arguments{
  \item{\dots}{SpatialPolygonsDataFrame(s) to rbind
  together}

  \item{fix.duplicated.IDs}{Whether to de-duplicate polygon
  IDs or not}
}
\value{
  SpatialPolygonsDataFrame
}
\description{
  rbind SpatialPolygonsDataFrames together, fixing IDs if
  duplicated
}
\author{
  Ari B. Friedman, with key functionality by csfowler on
  StackExchange
}

