\name{interpolatePathpoints}
\alias{interpolatePathpoints}
\title{Interpolate points along a path}
\usage{
  interpolatePathpoints(pathpoints, dens,
    tolerance.min = 1.2, tolerance.max = 50)
}
\arguments{
  \item{pathpoints}{Path points as they currently exist}

  \item{dens}{inverse density and is in the units of the x
  and y in pathpoints (e.g. 1 point per density meters)}

  \item{tolerance.min}{The proportion of the density (e.g.
  1.2 means we'll fill in gaps 20% greater than the density
  size)}

  \item{tolerance.max}{Max tolerance (see
  \code{tolerance.min} )}
}
\value{
  path with points interpolated
}
\description{
  Interpolate points along a path
}
\seealso{
  See Also \code{\link{reshapeSLDF}},
  \code{\link{SLDFtoLine}}
}

