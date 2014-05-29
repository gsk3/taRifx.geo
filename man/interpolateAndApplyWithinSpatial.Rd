\name{interpolateAndApplyWithinSpatial}
\alias{interpolateAndApplyWithinSpatial}
\title{Generic function to interpolate from a polygon to points lying inside it
This function was designed to solve the following problem.  Suppose you have counts
of the number of entities inside a polygon (N).  To compute distances to a point, you might
take the distance from the polygon centroid.  But this is too simplistic--it discards the
positional uncertainty inherent in not knowing the exact location of each entity which
makes up the count.  Instead, we repeatedly sample N points from the census block group centroids
which lie within our polygon, weight them by their population, and compute distances from there.}
\usage{
  interpolateAndApplyWithinSpatial(crude, fine, FUN,
    nSampleCol, samplesize = 30, simplify = FALSE, ...)
}
\arguments{
  \item{crude}{A SpatialPolygonsDataFrame containing the
  variable of interest}

  \item{fine}{an sp object whose points or polygons lie
  inside the polygons of crude}

  \item{FUN}{An function taking at least two arguments (a
  single polygon of crude in the first position, and the
  within-polygon elements of fine in the second)}

  \item{nSampleCol}{Column name in crude containing number
  of elements of fineWithin to sample per polygon}

  \item{samplesize}{The number of replicates per element of
  crude to draw}

  \item{simplify}{Whether to simplify to an array or not}

  \item{\dots}{Arguments to be passed to FUN}
}
\value{
  list of length length(crude) where each element is a list
  of length samplesize containing the results of FUN for
  that crude-element/sample
}
\description{
  Generic function to interpolate from a polygon to points
  lying inside it This function was designed to solve the
  following problem.  Suppose you have counts of the number
  of entities inside a polygon (N).  To compute distances
  to a point, you might take the distance from the polygon
  centroid.  But this is too simplistic--it discards the
  positional uncertainty inherent in not knowing the exact
  location of each entity which makes up the count.
  Instead, we repeatedly sample N points from the census
  block group centroids which lie within our polygon,
  weight them by their population, and compute distances
  from there.
}
\examples{
# Not run because too time-consuming
\dontrun{
require(fields)
require(rgdal)
distanceMatrix <- function( points1, points2, dist.fn=rdist.earth ) {
 cat( "Generating distance matrix for ",length(points1)," by ", length(points2), " matrix.\\n" )
 if(!is.na(proj4string(points1)))  points1 <- spTransform( points1, CRS("+proj=longlat +datum=WGS84") )
 if(!is.na(proj4string(points2)))  points2 <- spTransform( points2, CRS("+proj=longlat +datum=WGS84") )
}
# One option: Use the apply functionality
dist <- interpolateAndApplyWithinSpatial( crude=polySP, fine=pointSP, FUN=distanceMatrix, nSampleCol="z", samplesize=25,  points2=pointSP2, simplify=TRUE )
# Dist now is a list of 3 matrices, each with dimensions: N x length(pointSP2) x samplesize
# Each matrix represents N entities imputed from a single polygon, so we can actually simplify further
library(abind)
distmat <- do.call( Curry(abind, along=1), dist )
mindist <- apply( distmat, 3, function(x) { # For each realization of the 'world'
  apply( x,  )
} )
} # end of dontrun
}

