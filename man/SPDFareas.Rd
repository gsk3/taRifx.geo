\name{SPDFareas}
\alias{SPDFareas}
\title{Return areas of polygons in a SpatialPolygonsDataFrame}
\usage{
  SPDFareas(SPDF, colname = "AREA")
}
\arguments{
  \item{SPDF}{SpatialPolygonsDataFrame}

  \item{colname}{Name of the column in the data frame
  component of the SpatialPolygonsDataFrame in which to
  store the polygon areas}
}
\value{
  SpatialPolygonsDataFrame
}
\description{
  Get the areas stored in the polygons and return them in
  the dataframe slot
}

