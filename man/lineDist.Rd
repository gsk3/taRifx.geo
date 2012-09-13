\name{lineDist}
\alias{lineDist}
\title{Line distance in SpatialLinesDataFrame}
\usage{
  lineDist(SLDF, varname = "distances")
}
\arguments{
  \item{SLDF}{SpatialLinesDataFrame}

  \item{varname}{Character string containing name of
  variable to hold line distances.}
}
\value{
  Returns a SpatialLinesDataFrame
}
\description{
  Stores length of each line segment in a
  SpatialLinesDataFrame's data.frame
}

