\name{simpledist}
\alias{simpledist}
\title{Cartesian distance between points}
\usage{
simpledist(points)
}
\arguments{
  \item{points}{points is a 2x2 matrix, where columns are
  x,y and rows are the two points}
}
\value{
Distance, in same units as input
}
\description{
Cartesian distance between points
}
\examples{
points <- matrix(c(0,3,0,4),nrow=2)
simpledist(points)
}

