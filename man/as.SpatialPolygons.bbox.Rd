\name{as.SpatialPolygons.bbox}
\alias{as.SpatialPolygons.bbox}
\title{Convert a bounding box to a SpatialPolygons object
Bounding box is first created (in lat/lon) then projected if specified}
\usage{
  as.SpatialPolygons.bbox(bbox,
    proj4stringFrom = CRS("+proj=longlat +datum=WGS84"),
    proj4stringTo = NULL, interpolate = 0)
}
\arguments{
  \item{bbox}{Bounding box: a 2x2 numerical matrix of
  lat/lon coordinates (rownames must be c('lat','lon') and
  colnames must be c('min','max'))}

  \item{proj4stringFrom}{Projection string for the current
  bbox coordinates (defaults to lat/lon, WGS84)}

  \item{proj4stringTo}{Projection string, or NULL to not
  project}

  \item{interpolate}{If nonzero, the number of nodes per
  side to add in (helps maintain coverage if you're
  projecting)}
}
\value{
  A SpatialPolygons object of the bounding box
}
\description{
  Convert a bounding box to a SpatialPolygons object
  Bounding box is first created (in lat/lon) then projected
  if specified
}
\examples{
bb <- matrix(c(3,2,5,4),nrow=2)
rownames(bb) <- c("lon","lat")
colnames(bb) <- c('min','max')
as.SpatialPolygons.bbox( bb )
}
\seealso{
  \code{\link{clipToExtent}} which uses the output of this
  to clip to a bounding box
}

