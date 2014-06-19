\name{clipToExtent}
\alias{clipToExtent}
\title{Restrict to extent of a polygon
Currently does the sloppy thing and returns any object that has any area inside the extent polygon}
\usage{
clipToExtent(sp, extent)
}
\arguments{
  \item{sp}{Spatial object}

  \item{extent}{a SpatialPolygons object - any part of sp
  not within a polygon will be discarded}
}
\value{
A spatial object of the same type
}
\description{
Restrict to extent of a polygon Currently does the sloppy
thing and returns any object that has any area inside the
extent polygon
}
\examples{
set.seed(1)
require(rgdal)
require(sp)
P4S.latlon <- sp::CRS("+proj=longlat +datum=WGS84")
ply <- sp::SpatialPolygons(list(
   sp::Polygons(list(Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))), "s1"),
   sp::Polygons(list(Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))), "s2")
 ), proj4string=P4S.latlon)
pnt <- sp::SpatialPoints( matrix(rnorm(100),ncol=2)+2, proj4string=P4S.latlon )
# Make bounding box as Spatial Polygon
bb <- matrix(c(3,2,5,4),nrow=2)
rownames(bb) <- c("lon","lat")
colnames(bb) <- c('min','max')
bbSP <- as.SpatialPolygons.bbox(bb, proj4stringTo=P4S.latlon )
# Clip to extent
plyClip <- clipToExtent( ply, bbSP )
pntClip <- clipToExtent( pnt, bbSP )
# Plot
plot( ply )
plot( pnt, add=TRUE )
plot( bbSP, add=TRUE, border="blue" )
plot( plyClip, add=TRUE, border="red")
plot( pntClip, add=TRUE, col="red", pch="o")
}
\seealso{
\code{\link{as.SpatialPolygons.bbox}} to create a SP from a
bbox
}

