# Create spatial data for examples

require(sp)
Srs1 = Polygons(list(Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))), "s1")
Srs2 = Polygons(list(Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))), "s2")
Srs3 = Polygons(list(Polygon(cbind(c(5,1,2,5,5),c(0,0,2,2,0)))), "s3")

polySP <- SpatialPolygonsDataFrame( SpatialPolygons(list(Srs1,Srs2,Srs3)), 
                                  data.frame( z=1:3, row.names=c("s1","s2","s3") ) )

set.seed(123)
pointSP <- SpatialPointsDataFrame( 
  coords=data.frame(
    x=runif(25,bbox(polySP)["x","min"],bbox(polySP)["x","max"]),
    y=runif(25,bbox(polySP)["y","min"],bbox(polySP)["y","max"])
  ), 
  data=data.frame(pop=floor(runif(25)*10^5)) 
  )
pointSP2 <- SpatialPointsDataFrame( 
  coords=data.frame(
    x=runif(30,bbox(polySP)["x","min"],bbox(polySP)["x","max"]),
    y=runif(30,bbox(polySP)["y","min"],bbox(polySP)["y","max"])
  ), 
  data=data.frame(pop=floor(runif(30)*10^5)) 
)