# Miscellaneous functions to do GIS-like manipulation of spatial objects

# Ari Friedman
# Version 1.0.4
# 8/02/2012


#'Cartesian distance between points
#'
#'@aliases simpledist
#'@param points points is a 2x2 matrix, where columns are x,y and rows are the
#'two points
#'@return Distance, in same units as input
#'@export simpledist
#'@examples
#'
#'points <- matrix(c(0,3,0,4),nrow=2)
#'simpledist(points)
#'
simpledist = function(points) {
	distance = sqrt( ( points[1,1] - points[2,1] )^2 + ( points[1,2] - points[2,2] )^2 )
	return(distance)
}
#' Find closest point to a given point's coordinates (closestPoint).
#'
#'@aliases closestPoint
#'@param points points is an Nx2 matrix, where columns are x,y and rows are the
#'two points
#'@param point the point from which you want to find the closest match in points
#'@return Distance, in same units as input
#'@export closestPoint
closestPoint = function(point,points) {
  distance = sqrt( ( points[,1] - point[[1]] )^2 + ( points[,2] - point[[2]] )^2 )
  mindist_selector = (distance==min(distance))
  if( length(mindist_selector[mindist_selector==TRUE]) != 1) { # Return NA if there are 2+ points at exactly the same distance
    return(NA)
  } else {
    return(mindist_selector)
  }
}


#'Convert a SpatialLinesDataFrame to a single line matrix with associated segment information
#'
#'@param lineDF SpatialLinesDataFrame object
#'@param orderXY ordering
#'@param segments segments to associate
#'@return data.frame
#'@seealso See Also \code{\link{reshapeSLDF}}
#'@export SLDFtoLine
#' 
SLDFtoLine = function(lineDF,orderXY=FALSE,segments=TRUE) {
	# Initialize variables
	X = c()
	Y = c()
	Segment = c()
	# Assemble the complete line segment
	numsegments = length(lineDF@lines)
	
	for(seg in 1:numsegments) {
		xnew=lineDF@lines[[seg]]@Lines[[1]]@coords[,1]
		ynew=lineDF@lines[[seg]]@Lines[[1]]@coords[,2]
	
		X = append(X,xnew)
		Y = append(Y,ynew)
		if(segments) {
			Segment = append(Segment,rep.int(seg,length(xnew)))
		}
		else {
			Segment = append(Segment,rep.int(1,length(xnew)))
		}
	}

	# Order the points in increasing X,Y order
	if(orderXY) {
		Y = Y[order(X)]
		Segment = Segment[order(X)]
		X = X[order(X)]
	}
	

	line = data.frame(X,Y,Segment)
	line

}


#'Smooth the line segments in a SpatialLinesDataFrame
#'
#'Smooth the line segments in a SpatialLinesDataFrame.  Takes a jittery SLDF
#'(such as a GPS produces) and smooths it.
#'
#'
#'@param lineDF SpatialLinesDataFrame
#'@return SpatialLinesDataFrame
#'@seealso See Also as \code{\link{SLDFtoLine}}, ~~~
#'@export smoothLines
#'
smoothLines=function(lineDF) {
	# Initialize variables
	#lineX = c()
	#lineY = c()
	#lineSeg = c()
	
	# Assemble the complete line segment
	numsegments = length(lineDF@lines)
	#-Not using this currently
	#for(seg in 1:numsegments) {
	#	lineX = append(lineX,lineDF@lines[[seg]]@Lines[[1]]@coords[,1])
	#	lineY = append(lineY,lineDF@lines[[seg]]@Lines[[1]]@coords[,2])
	#	lineSeg = append(lineSeg,rep.int(seg,length(lineDF@lines[[seg]]@Lines[[1]]@coords[,2])))
	#}
	#spline=sreg(lineX[lineSeg==seg],lineY[lineSeg==seg])
	#lineX[lineSeg==seg]=spline$predicted$x
	#lineY[lineSeg==seg]=spline$predicted$y
	
	# Break the line segment at key points (borrow this from the SNAKES techniques)
	
	# Take our new line segments and smooth them
	for(seg in 1:numsegments) {
		#Order them for the splines
		plot(x,y,main=paste(seg))
		x = lineDF@lines[[seg]]@Lines[[1]]@coords[,1][order(lineDF@lines[[seg]]@Lines[[1]]@coords[,1])]
		y = lineDF@lines[[seg]]@Lines[[1]]@coords[,2][order(lineDF@lines[[seg]]@Lines[[1]]@coords[,1])]
		#Fit and return
		if(length(x)>4) {
			fit <- smooth.Pspline(x, y, method=3)
		}
		fit
		#spline=sreg(lineDF@lines[[seg]]@Lines[[1]]@coords[,1],lineDF@lines[[seg]]@Lines[[1]]@coords[,2])
		#lineDF@lines[[seg]]@Lines[[1]]@coords[,1] = spline$predicted$x
		#lineDF@lines[[seg]]@Lines[[1]]@coords[,2] = spline$predicted$y
	}
}



#'Take a grid of regularly spaced points (such as those output by the centroids
#'of Arc's fishnet function) and convert it to various grid data types
#'
#'
#'@param df SpatialPointsDataFrame
#'@param type "SpatialGrid" or "SpatialPolygons" or "SpatialPolygonsDataFrame"
#'@return SpatialGrid, SpatialPolygons, SpatialPolygonsDataFrame
#'@export pointgrid2SpatialPolygons
#'
pointgrid2SpatialPolygons=function(df,type) {
	# Check that df is a SpatialPointsDataFrame
	if (class(df)[[1]] != "SpatialPointsDataFrame" & class(df)[[1]] !="SpatialPixelsDataFrame") {
		return(-99)
	}
	
	#Output SpatialGrid
	if (type=="SpatialGrid") {
		return(SpatialGrid(points2grid(df)));
	}
	
	#Output SpatialPolygons(DataFrame)
	df_sp=as.SpatialPolygons.GridTopology(points2grid(df))
	if(type=="SpatialPolygons") {
		return (df_sp)
	}
	if(type=="SpatialPolygonsDataFrame") {
		data=as.data.frame(rep(0,length(df_sp@plotOrder)))
		rownames(data)<-getSpPPolygonsIDSlots(df_sp)
		return(SpatialPolygonsDataFrame(df_sp,data))
	}
	
	#Otherwise
	return(-98)
}



#'Subset SpatialPolygonsDataFrame or SpatialPointsDataFrame
#'
#'
#'@aliases subsetSPDF subset.SpatialPolygonsDataFrame
#'subset.SpatialPointsDataFrame
#'@param x SpatialPolygonsDataFrame or SpatialPointsDataFrame
#'@param tf Boolean on which to subset
#'@param \dots Arguments to pass to SpatialPolygonsDataFrame() or
#'SpatialPointsDataFrame() when reconstructing objects
#'@return SpatialPolygonsDataFrame or SpatialPointsDataFrame
#'@export subsetSPDF subset.SpatialPolygonsDataFrame
#'
subsetSPDF = function(x,tf,...) {
	selected_data <- subset(x@data, tf)
	if(class(x)=="SpatialPolygonsDataFrame") {
		SPDF_selected <- subset(x@polygons, tf)
		centroids <- getSpPPolygonsLabptSlots(as.SpatialPolygons.PolygonsList(SPDF_selected))
		xs <- centroids[,1]
		ys <- centroids[,2]
		export <- SpatialPolygonsDataFrame(as.SpatialPolygons.PolygonsList(SPDF_selected),data=data.frame(x=xs, y=ys,row.names=getSpPPolygonsIDSlots(as.SpatialPolygons.PolygonsList(SPDF_selected)),selected_data),...)
	}
	if(class(x)=="SpatialPointsDataFrame") {
		export <- SpatialPointsDataFrame(coordinates(x)[tf,],data=selected_data,coords.nrs=x@coords.nrs,proj4string=x@proj4string,...)
	}
	return(export)
}
subset.SpatialPolygonsDataFrame <- function(x,tf,...) {
  subsetSPDF(x,tf,...)
}
subset.SpatialPointsDataFrame <- function(x,tf,...) {
  subsetSPDF(x,tf,...)
}


#'Return areas of polygons in a SpatialPolygonsDataFrame
#'
#'Get the areas stored in the polygons and return them in the dataframe slot
#'
#'
#'@param SPDF SpatialPolygonsDataFrame
#'@param colname Name of the column in the data frame component of the
#'SpatialPolygonsDataFrame in which to store the polygon areas
#'@return SpatialPolygonsDataFrame
#'@export SPDFareas
#'
SPDFareas = function(SPDF,colname="AREA") {
	numPolys <- length(SPDF@polygons)
	areas <- data.frame(rep(NA,numPolys))
	colnames(areas) <- c(colname)
	for(polyNum in 1:numPolys) {
		areas[[colname]][polyNum] <- SPDF@polygons[[polyNum]]@area
	}
	returnSPDF = SpatialPolygonsDataFrame(polygons(SPDF),data=cbind(SPDF@data,areas),match.ID=TRUE)
  return(returnSPDF)
}


#'Count points within a polygon
#'
#'Overlays points on polygons and create a new polygon dataset with the count
#'of the points in that polygon
#'
#'
#'@param points SpatialPoints
#'@param polys SpatialPolygonsDataFrame
#'@param density Return a density (point count divided by area) instead of a
#'point count
#'@param by Factor to return counts by.  For instance, if by is 
#'a factor with two levels, instead of a single count variable being returned, two variables will be returned--
#'the count of point type A in the polygon, and the count of point type B.  The by factor must be of length length(points).
#'@return SpatialPolygonsDataFrame
#'@seealso See Also as \code{\link[sp]{overlay}}
#'@export countPointsInPolys
countPointsInPolys = function(points,polys,density=FALSE,by=NULL) {
  countpoints <- function(points,polys) {
    overlay <- over( points, polys, returnList=TRUE) # need to use `,returnList` to get the rownames to be the polygon IDs
    overlayIDs <- unlist(lapply( overlay, function(x) rownames(x) )) # vector of polygon IDs, repeated if there's more than one point in the polygon
    unclass(table(overlayIDs))
  }
  if(is.null(by)) {
    pointcount <- countpoints(points,polys)
    pointcount.df <- data.frame( pointcount=pointcount, rownames=rownames(pointcount), stringsAsFactors=FALSE )
  } else { # count by levels
    lvls <- levels(by)
    for(l in lvls) {
      pts <- subset(points,by==l)
      pointcount <- countpoints(pts,polys)
      pointcount.new <- data.frame( pointcount=pointcount, rownames=rownames(pointcount), stringsAsFactors=FALSE )
      colnames(pointcount.new)[1] <- paste0("pointcount.",l)
      if(l==lvls[1]) {
        pointcount.df <- pointcount.new
      } else{
        pointcount.df <- merge(pointcount.df,pointcount.new,all.x=TRUE,all.y=TRUE,by="rownames")
      }
    }
  }
  #- Merge with polygons data.frame
  polys.df <- data.frame(polys@data,rownames=rownames(polys@data),stringsAsFactors=FALSE)
  DF <- merge(polys.df,pointcount.df,all.x=TRUE,by.x="rownames")
  if(nrow(DF)!=nrow(polys@data)) stop("Rows created/deleted while merging in the count data.")
  DF <- DF[order(as.numeric(DF$rownames)),] # sort it
  rownames(DF) <- DF$rownames
  DF <- DF[,!colnames(DF) %in% "rownames"]
  pc.cols <- grepl("pointcount",colnames(DF)) # selector for pointcount column(s)
  DF[,pc.cols] <- sapply( DF[,pc.cols], function(x) {
    x <- as.numeric(x)
    x[is.na(x)] <- 0
    x
  } )
  SpatialPolygonsDataFrame(polygons(polys),data=DF,match.ID=TRUE)
}

#'Count polygons within other polygons
#'
#'Overlays polygons on other polygons and create a new polygon dataset with the count
#'of the points in that polygon
#'
#'
#'@param polys1 SpatialPolygonsDataFrame
#'@param polys2 SpatialPolygonsDataFrame
#'@return SpatialPolygonsDataFrame
#'@seealso See Also as \code{\link[sp]{overlay}}
#'@export countPolysInPolys
countPolysInPolys = function(points,polys,density=FALSE,by=NULL) {
  
}

#'Reshape a spatialLinesDataFrame into a series of points with associated
#'information
#'
#'Reshape a spatialLinesDataFrame into a series of points with associated
#'information (less efficient because all the segment data gets replicated over
#'each point)
#'
#'
#'@param SLDF spatialLinesDataFrame
#'@param shape Do not change.  Must be "long".  For forward compatility.
#'@return data.frame
#'@seealso See Also \code{\link{SLDFtoLine}}
#'@export reshapeSLDF
reshapeSLDF = function(SLDF,shape="long") {
	if(shape=="long"){
		# Loop over each segment
		for(seg in 1:length(SLDF@lines)) {
			numRecords = length(SLDF@lines[[seg]]@Lines[[1]]@coords[,1])
			# Longit
			longit = SLDF@lines[[seg]]@Lines[[1]]@coords[,1]
			# Latit
			latit = SLDF@lines[[seg]]@Lines[[1]]@coords[,2]
			# Data
			# Initialize variables
			SLDFdata = as.data.frame(replicate(length(SLDF@data),rep(NA,numRecords)))
			colnames(SLDFdata) <- colnames(SLDF@data)
			for(datacolnum in 1:length(SLDF@data)) {	
				SLDFdata[[datacolnum]]=rep(SLDF@data[[datacolnum]][seg],numRecords)
			}
			
			# Store our results
			if(seg==1) {
				SLDFpoints=cbind(longit,latit,SLDFdata)
			}	else {
				SLDFpoints=rbind(SLDFpoints,cbind(longit,latit,SLDFdata))
			}
		}
		return(SLDFpoints)
	}
	
}


#'Interpolate points along a path
#'
#'
#'@param pathpoints Path points as they currently exist
#'@param dens inverse density and is in the units of the x and y in pathpoints
#'(e.g. 1 point per density meters)
#'@param tolerance.min The proportion of the density (e.g. 1.2 means we'll fill
#'in gaps 20% greater than the density size)
#'@param tolerance.max Max tolerance (see \code{tolerance.min} )
#'@return path with points interpolated
#'@seealso See Also \code{\link{reshapeSLDF}}, \code{\link{SLDFtoLine}}
#'@export interpolatePathpoints
#'
interpolatePathpoints = function(pathpoints,dens,tolerance.min=1.2,tolerance.max=50) {
  require(taRifx)
	# dens actually inverse density and is in the units of the x and y in pathpoints (e.g. 1 point per density meters)
	# tolerance.min is in the proportion of the density (e.g. 1.2 means we'll fill in gaps 20% greater than the density size)

	# - Compute distance to next point - #
	#(shift the entire matrix down one row and calculate the distance all at once)
	pathpoints$xNext=c(pathpoints$x[2:length(pathpoints$x)],NA)
	pathpoints$yNext=c(pathpoints$y[2:length(pathpoints$y)],NA)
	pathpoints$distToNext = sqrt((pathpoints$x-pathpoints$xNext)^2 + (pathpoints$y-pathpoints$yNext)^2)
	
	# -- If distance to next point exceeds the dens by the tolerance, interpolate -- #
	# - First group them into chunks to add on our segments to the end of - #
	interpFlag=rep(0,length(pathpoints$x))
	interpFlag[pathpoints$distToNext>dens*tolerance.min & pathpoints$distToNext<dens*tolerance.max]=1
	# Shift the flag one back so our points are at the end of the group
	interpolate=rep(0,length(interpFlag))
	interpolate[2:length(interpFlag)]=interpFlag[1:length(interpFlag)-1]
	# Code the groups
	pathpoints$interpolateGroup=rep(1,length(pathpoints$x))
	for(rownum in 2:length(interpolate)) {
		pathpoints$interpolateGroup[rownum]=pathpoints$interpolateGroup[rownum-1]+interpolate[rownum]
	}
	# Flag the last group so it can be skipped for interpolation #
	numIGroups=max(pathpoints$interpolateGroup)
	# - Add our interpolated points on to the end - #
	bypp=by(pathpoints,pathpoints$interpolateGroup,function(pp) {
		if(pp$interpolateGroup[1]!=numIGroups) { # Skip the last group since it's last record is NA
			numRows = length(pp[[1]]) # get the index of the last row
			pointsToAdd = as.integer(floor(pp$distToNext[numRows] / dens)) # number of points to add
			xIncrement = ((pp$xNext - pp$x) / (pointsToAdd+1))[numRows]
			yIncrement = ((pp$yNext - pp$y) / (pointsToAdd+1))[numRows]
			pp <- expandDF(pp,numRows,pointsToAdd)

			for(pointNum in 1:pointsToAdd) {
				pp[numRows+pointNum,"x"] = pp[numRows+pointNum-1,"x"] + xIncrement
				pp[numRows+pointNum,"y"] = pp[numRows+pointNum-1,"y"] + yIncrement
			}
		}
		return(pp)
	})
	# Now assemble all and return
	returnpp=bypp[[1]]
	for(igroup in 2:numIGroups) {
		returnpp=rbind(returnpp,bypp[[igroup]])
	}
	
	#subset(,select)
	return(returnpp)
}



#'Standardize latitude/longitude coordinates
#'
#'
#'@param vec Character vector of lat/long coordinates
#'@return Numeric decimal lat/lon
#'@export cleanLatLon
#'
cleanLatLon = function(vec) {
	if(!is.character(vec))		stop("Input vector must be of type character")
	vec = sub("[NnEe]","",vec)
	vec = sub("[WwSs]","-",vec)
	vec = sub("^ +","",vec)
	vec = sub(" +$","",vec)
	vec = sub("^0+","",vec)
	if(length(grep(" ",vec))!=0)		stop("Your vector appears to be in decimal degrees, which are not yet implemented")
	return(as.numeric(vec))
}


#'Calculate cumulative distance along a matrix of x,y coordinates
#'
#'
#'@param coords an [n,2] matrix of coordinates
#'@return A single value that represents the distance along the path
#'@seealso See Also \code{\link{reshapeSLDF}}, \code{\link{SLDFtoLine}}
#'@export cumDist
cumDist = function(coords) {
	# coords should be an [n,2] matrix of coordinates
	
	if(any(is.na(coords))) {
		return(NA)
	}
	
	if(dim(coords)[1] == 2) {
		return(simpledist(rbind(coords[1,],coords[2,])))
	}
	
	prevCoords = coords[1:dim(coords)[1]-1,]
	currCoords = coords[2:dim(coords)[1],]
	distToPrev = rep(NA,dim(coords)[1])
	for(rowNum in 2:dim(coords)[1]) {
		distToPrev[rowNum] = simpledist(rbind(prevCoords[(rowNum-1),],currCoords[(rowNum-1),]))
	}
	return(sum(distToPrev,na.rm=TRUE))
}


#'Line distance in SpatialLinesDataFrame
#'
#'Stores length of each line segment in a SpatialLinesDataFrame's data.frame
#'
#'
#'@param SLDF SpatialLinesDataFrame
#'@param varname Character string containing name of variable to hold line
#'distances.
#'@return Returns a SpatialLinesDataFrame
#'@export lineDist
lineDist = function(SLDF, varname="distances") {
	numSegments = length(SLDF@lines)
	Dists = rep(NA,numSegments)
	for(segnum in 1:numSegments) {
		Dists[segnum] <- cumDist(SLDF@lines[[segnum]]@Lines[[1]]@coords)
	}
	SLDF@data[[varname]] = Dists
	return(SLDF)
}



#'Create all pairwise distances of points from a SpatialPointsDataFrame
#'
#'
#'@param SPDF SpatialPointsDataFrame
#'@param names variable name in the SPDF's dataframe used to label each point
#'in the resulting matrix
#'@return matrix
#'@export pointDistPairwise
pointDistPairwise = function(SPDF, names = "name") {
	crds <- SPDF@coords #Coordinate data
	# Set up our matrix
	numpoints <- dim(crds)[1]
	pw.mat <- matrix(rep(NaN,numpoints^2),ncol=numpoints)
	for(i in seq(numpoints)) {
		for(j in seq(numpoints)) {
			if(i>=j) {
				pw.mat[i,j] <- simpledist(rbind(crds[i,],crds[j,]))
			}
		}
	}
	colnames(pw.mat) <- SPDF@data[[names]]
	rownames(pw.mat) <- SPDF@data[[names]]
	return(pw.mat)
}

#'Convert SpatialPointsDataFrame to a regular data.frame with the coordinates
#'as "x" and "y"
#'
#'
#'@param SPDF SpatialPointsDataFrame
#'@return data.frame with the coordinates as "x" and "y"
#'@export SPDFtoPointsDF
SPDFtoPointsDF <- function(SPDF) {
	coords <- SPDF@coords
	colnames(coords) <- c("x","y")
	return(cbind(SPDF@data,coords))
}

#' Get sp feature IDs
#' @aliases IDs IDs.default IDs.SpatialPolygonsDataFrame
#' @param x The object to get the IDs from
#' @param \dots Pass-alongs
#' @author Ari B. Friedman
#' @rdname IDs
IDs <- function(x,...) {
  UseMethod("IDs",x)
}
#' @method IDs default
#' @S3method IDs default
#' @rdname IDs
IDs.default <- function(x,...) {
  stop("Currently only SpatialPolygonsDataFrames are supported.")
}
#' @method IDs SpatialPolygonsDataFrame
#' @S3method IDs SpatialPolygonsDataFrame
#' @rdname IDs
IDs.SpatialPolygonsDataFrame <- function(x,...) {
  vapply(slot(x, "polygons"), function(x) slot(x, "ID"), "")
}

#' Assign sp feature IDs
#' @aliases IDs<- IDs.default<-
#' @param x The object to assign to
#' @param value The character vector to assign to the IDs
#' @author Ari B. Friedman
#' @rdname IDs
"IDs<-" <- function( x, value ) {
  UseMethod("IDs<-",x)
}
#' @method IDs<- SpatialPolygonsDataFrame
#' @S3method IDs<- SpatialPolygonsDataFrame
#' @rdname IDs
"IDs<-.SpatialPolygonsDataFrame" <- function( x, value) {
  spChFIDs(x,value)
}

#' rbind SpatialPolygonsDataFrames together, fixing IDs if duplicated
#' @param \dots SpatialPolygonsDataFrame(s) to rbind together
#' @param fix.duplicated.IDs Whether to de-duplicate polygon IDs or not
#' @return SpatialPolygonsDataFrame
#' @author Ari B. Friedman, with key functionality by csfowler on StackExchange
#' @method rbind.SpatialPolygonsDataFrame
#' @export rbind.SpatialPolygonsDataFrame
rbind.SpatialPolygonsDataFrame <- function(..., fix.duplicated.IDs=TRUE) {
  dots <- as.list(substitute(list(...)))[-1L]
  dots_names <- as.character(dots) # store names of objects passed in to ... so that we can use them to create unique IDs later on
  dots <- lapply(dots,eval)
  names(dots) <- NULL
  # Check IDs for duplicates and fix if indicated
  IDs_list <- lapply(dots,IDs)
  dups.sel <- duplicated(unlist(IDs_list))
  if( any(dups.sel) ) {
    if(fix.duplicated.IDs) {
      dups <- unique(unlist(IDs_list)[dups.sel])
      # Function that takes a SPDF, a string to prepend to the badID, and a character vector of bad IDs
      fixIDs <- function( x, prefix, badIDs ) {
        sel <-  IDs(x) %in% badIDs
        IDs(x)[sel] <- paste( prefix, IDs(x)[sel], sep="." )
        x
      }
      dots <- mapply(FUN=fixIDs , dots, dots_names, MoreArgs=list(badIDs=dups) )
    } else {
      stop("There are duplicated IDs, and fix.duplicated.IDs is not TRUE.")
    }
  } else { # Confirm that IDs match associated data.frame rownames
    broken_IDs <- vapply( dots, function(x) all(IDs(x)!=rownames(x@data)), FALSE )
    if( any(broken_IDs) ) {
      for( i in which(broken_IDs) ) {
        rownames( dots[[i]]@data ) <- IDs(dots[[i]])
      }
    }
  }
  # One call to bind them all
  pl = do.call("rbind", lapply(dots, function(x) as(x, "SpatialPolygons")))
  df = do.call("rbind", lapply(dots, function(x) x@data))
  SpatialPolygonsDataFrame(pl, df)
}

#' Merge a SpatialPolygonsDataFrame with a data.frame
#' @param SPDF A SpatialPolygonsDataFrame
#' @param df A data.frame
#' @param \dots Parameters to pass to merge.data.frame
#' 
#' @export
#' @docType methods
#' @rdname merge-methods
setGeneric("merge", function(x, y, ...){
  standardGeneric("merge")
}, useAsDefault=base::merge)
#' @rdname merge-methods
#' @aliases merge,SpatialPolygonsDataFrame,data.frame-method
setMethod(
  f="merge",
  signature=c("SpatialPolygonsDataFrame","data.frame"), 
  definition=function(x,y,...) {
    nrowBefore <- nrow(x@data)
    x$.rowNames <- rownames(x@data)
    x$.sortOrder <- seq(nrow(x))
    newDF <- merge.data.frame( x@data, y, all.x=TRUE, all.y=FALSE, sort=FALSE, ... )
    nrowAfter <- nrow(newDF)
    stopifnot(nrowBefore==nrowAfter)
    x@data <- newDF
    x@data <- x@data[ order(x$.sortOrder), ]
    rownames(x@data) <- x$.rowNames
    x@data <- x@data[, !colnames(x@data) %in% c(".rowNames",".sortOrder")  ]
    return(x)
})