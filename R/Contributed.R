### Various contributed functions


#'Geocode character vectors using online services
#'
#'Geocode character vectors (or data.frames) using Google or Bing's API
#'
#'@aliases geocode geocode.default geocode.data.frame
#'@param x A vector or data.frame
#'@param verbose Whether to display each address as it is submitted to Google or not
#'@param service API to use.  Current options are "bing" or "google".  To add your Bing API key, set options(BingMapsKey="yourkeygoeshere").
#'@param returntype What to return.  Options include "coordinates" and "zip".
#'@param addresscol A (character) name of the column in a data.frame which contains the addresses
#'@param \dots Other items to pass along
#'@import RJSONIO
#'@return geocode.default returns a numeric vector of length 2 containing the
#'latitudes and longitudes. geocode.data.frame returns the original data.frame
#'with two additional columns for the longitude and latitudes.
#'@author Error handling, object orientation, and Bing
#' capabilities by Ari Friedman. Google REST algorithm by Tony Breyal (http://stackoverflow.com/a/3259537/636656). 
#'@examples
#'\dontrun{
#'geocode("3817 Spruce St, Philadelphia, PA 19104")
#'geocode("Philadelphia, PA")
#'dat <- data.frame(
#'  value=runif(3),
#'  address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland")
#')
#'geocode(dat)
#'}
#'@rdname geocode
#'@export geocode
geocode <- function( x, verbose=FALSE, service="google", returntype="coordinates", ... ) {
  UseMethod("geocode",x)
}
#'@rdname geocode
#'@method geocode default
#'@S3method geocode default
geocode.default <- function(x,verbose=FALSE, service="google", returntype="coordinates", ...) {
  if(x=="") return(c(NA,NA))
  # Input regularization and checking
  service <- tolower(service)
  BingMapsKey <- getOption("BingMapsKey")
  if(service=="bing" && is.null(BingMapsKey) ) stop("To use Bing, you must save your Bing Maps API key (obtain at http://msdn.microsoft.com/en-us/library/ff428642.aspx) using options(BingMapsKey='mykey').\n")
  # URL constructing
  construct.geocode.url <- list()
  construct.geocode.url[["google"]] <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))  
  }
  construct.geocode.url[["bing"]] <- function(address, maxResults=1) { # documented at http://msdn.microsoft.com/en-us/library/ff701711
    root <- "http://dev.virtualearth.net/REST/v1/Locations"
    u <- paste0(root, "?query=", address, "&maxResults=",maxResults,"&key=",BingMapsKey)
    return(URLencode(u))
  }
  if(verbose) message(x,appendLF=FALSE)
  u <- construct.geocode.url[[service]](x)
  doc <- RCurl::getURL(u)
  j <- RJSONIO::fromJSON(doc,simplify = FALSE)
  # Parse and return
  parse.json <- list()
  parse.json[["google"]] <- function(j) {
    if(j$status=="OK") {
      res <- list()
      if( "coordinates" %in% returntype ) {
        lat <- j$results[[1]]$geometry$location$lat
        lng <- j$results[[1]]$geometry$location$lng
        res$coordinates <- c(lat, lng)
      }
      if( "zip" %in% returntype )  {
        zp <- j$results[[1]]$address_components[[8]]$short_name
        if( j$results[[1]]$address_components[[8]]$types[[1]] != "postal_code" )  warning(paste("Not sure these zips are actually zips.  Type:", j$results[[1]]$address_components[[8]]$types[[1]]) )
        res$zip <- zp
      }
      return( res )
    } else {
      if(j$status=="OVER_QUERY_LIMIT") warning("Google's geocoding quota appears to have been reached for the day.")
      return(c(NA,NA))
    }
  }
  parse.json[["bing"]] <- function(j) {
    if(j$authenticationResultCode != "ValidCredentials") {
      warning("Your BingMapsKey was not accepted.")
      return(c(NA,NA))
    }
    if(j$statusDescription!="OK") {
      warning("Something went wrong. Bing Maps API return status code ",j$statusCode," - ", j$statusDescription)
      return(c(NA,NA))
    }
    if(j$resourceSets[[1]]$estimatedTotal==0) {
      warning("Didn't find any points")
      return(c(NA,NA))
    }
    if(verbose) message(" - Confidence: ", j$resourceSets[[1]]$resources[[1]]$confidence ,appendLF=FALSE)
    res <- list()
    if( "coordinates" %in% returntype ) {
      crds <- unlist(j$resourceSets[[1]]$resources[[1]]$point$coordinates)
      res$coordinates <- crds
    }
    if( "zip" %in% returntype )  {
      res$zip <- sub( "^.*(\\d{5}-?\\d?\\d?\\d?\\d?).*$", "\\1", j$resourceSets[[1]]$resources[[1]]$address$formattedAddress )
    }
    return( res )
  }
  res <- parse.json[[service]](j)
  if(length(returntype)==1) res <- res[[1]]
  if(verbose) message("\n",appendLF=FALSE)
  return( res )
}
#'@rdname geocode
#'@method geocode data.frame
#'@S3method geocode data.frame
geocode.data.frame <- function(x, verbose=FALSE, service="google", returntype="coordinates", addresscol="address", ... ) {
  if("zip" %in% returntype )  stop("data.frame geocoding only supports coordinates return type")
  # Ignore any rows that have already been geocoded
  already.geocoded <- "lat" %in% colnames(x) & "lon" %in% colnames(x)
  if( already.geocoded ) {
    sel <- is.na(x$lat) | is.na(x$lon)
    cat("This data.frame has already been partially geocoded.  Geocoding",sum(sel),"out of",nrow(x),"addresses.\n")
  } else{
    sel <- rep(TRUE,nrow(x))
  }
  # Geocode
  gcRobust <- function(a) {
    res <- try(geocode(a, verbose = verbose, service = service, ...))
    if ( class(res) == "try-error" | ( is.na(res) & length(res)==1 ) )
      res <- c(NA, NA)
    res
  }
  latlon <- taRifx::stack.list( lapply(x[[addresscol]][sel],gcRobust ))
  rownames(latlon) <- NULL
  latlonDF <- as.data.frame(latlon)
  colnames(latlonDF) <- c("lat","lon")
  # Return result
  if(already.geocoded) {
    x$lat[sel] <- latlonDF$lat
    x$lon[sel] <- latlonDF$lon
    return(x)
  } else{
    return( cbind( x, latlonDF ) )
  }
}

#'Geocode using Google Maps API (deprecated)
#'This has been generalized to the taRifx::geocode() function
#'@param \dots Ignored
#'@export gGeoCode
gGeoCode <- function(...) {
  .Deprecated("geocode","taRifx","gGeoCode has been generalized to other services.  Enjoy!")
}


#'Find driving routes using online services
#'
#'Find transit routes using Google or Bing's API
#'
#'@aliases georoute georoute.default
#'@param x A character vector of length>=2, where each element is a (starting, ending, or intermediate) waypoint, or a numeric matrix with columns c('lat','lon') where each row is a waypoint
#'@param verbose Provide additional information
#'@param returntype What information to return.  Currently, the options are "all", "distance", "distanceUnit", "path", "time", and/or "timeUnit".  Can be combined, as in returntype=c("time","distance").
#'@param service API to use.  Currently the only option is "bing"
#'@param \dots Other items to pass along
#'@return Route information (see the returntype argument)
#'@author Ari B. Friedman
#'@examples
#'\dontrun{
#'georoute( c("3817 Spruce St, Philadelphia, PA 19104", 
#'  "9000 Rockville Pike, Bethesda, Maryland 20892"), verbose=TRUE )
#'georoute( c("3817 Spruce St, Philadelphia, PA 19104", 
#'  "Tulum, MX","9000 Rockville Pike, Bethesda, Maryland 20892"), returntype="distance" )
#'georoute( c("3817 Spruce St, Philadelphia, PA 19104", 
#'  "9000 Rockville Pike, Bethesda, Maryland 20892"), verbose=TRUE, returntype="path" )
#'georoute( c("3817 Spruce St, Philadelphia, PA 19104", 
#'  "9000 Rockville Pike, Bethesda, Maryland 20892"), verbose=TRUE, returntype="time" )
#'# Using lat/lon
#'xmat <- rbind( 
#'  geocode( "3817 Spruce St, Philadelphia, PA 19104" ), 
#'  geocode( "9000 Rockville Pike, Bethesda, Maryland 20892" ) 
#')
#'colnames(xmat) <- c( 'lat', 'lon' )
#'georoute( xmat, verbose=TRUE, returntype = c("distance","distanceUnit") )
#'}
#'@rdname georoute
#'@export georoute
georoute <- function( x, verbose=FALSE, service="bing", returntype="all", ... ) {
  UseMethod("georoute",x)
}
#'@rdname georoute
#'@method georoute default
#'@S3method georoute default
georoute.default <- function( x, verbose=FALSE, service="bing", returntype="all", ...) {
  # Input regularization and checking
  service <- tolower(service)
  BingMapsKey <- getOption("BingMapsKey")
  if(service=="bing" && is.null(BingMapsKey) ) stop("To use Bing, you must save your Bing Maps API key (obtain at http://msdn.microsoft.com/en-us/library/ff428642.aspx) using options(BingMapsKey='mykey').\n")
  # URL constructing
  construct.georoute.url <- list()
  construct.georoute.url[["bing"]] <- function(waypoints, maxSolutions=1, optimize="time", distanceUnit="km",travelMode="Driving",path=(returntype=="path") ) { # documented at http://msdn.microsoft.com/en-us/library/ff701717
    if( "data.frame" %in% class(waypoints) )  waypoints <- as.matrix(waypoints)
    if( class(waypoints) == "matrix" ) { # handle lat/lon cases by converting to character strings separated by commas (e.g. 42.5,-77 gets converted to "42.5,-77" for use in the URL)
      waypoints <- apply( waypoints, 1, paste0, collapse="," )
    }
    root <- "http://dev.virtualearth.net/REST/v1/Routes"
    waypointsquery <- paste("wayPoint.",seq_along(waypoints),"=",waypoints,collapse="&",sep="")
    routePathOutputquery <- ifelse(path,"&routePathOutput=Points","")
    u <- paste0(root, "?", waypointsquery, "&maxSolutions=",maxSolutions,"&optimize=",optimize,
                routePathOutputquery,"&distanceUnit=",distanceUnit,"&travelMode=",travelMode,
                "&key=",BingMapsKey)
    return(URLencode(u))
  }
  if(verbose) message(x,appendLF=FALSE)
  u <- construct.georoute.url[[service]](x)
  doc <- RCurl::getURL(u)
  j <- RJSONIO::fromJSON(doc,simplify = FALSE)
  # Parse and return
  parse.json <- list()
  parse.json[["bing"]] <- function(j) {
    if(j$authenticationResultCode != "ValidCredentials") stop("Your BingMapsKey was not accepted.\n")
    if(j$statusDescription!="OK") stop("Something went wrong. Bing Maps API return status code ",j$statusCode," - ", j$statusDescription,"\n")
    rt <- j$resourceSets[[1]]$resources[[1]]
    if(verbose) message(" - Confidence: ", rt$routeLegs[[1]]$startLocation$confidence,appendLF=FALSE)
    if(verbose) message(" - Distance unit: ", rt$distanceUnit, " Time unit:", rt$durationUnit ,appendLF=FALSE)
    if( "all" %in% returntype ) {
      res <- rt$routeLegs[[1]]
    } else {
      res <- list()
      if( "path" %in% returntype ) res[[ "path" ]] <-  t(sapply(rt$routePath$line$coordinates, unlist))
      if( "distance" %in% returntype ) res[[ "distance" ]] <-  rt$travelDistance
      if( "distanceUnit" %in% returntype ) res[[ "distanceUnit" ]] <- rt$distanceUnit
      if( "time" %in% returntype ) res[[ "time" ]] <- rt$travelDuration
      if( "timeUnit" %in% returntype ) res[[ "timeUnit" ]] <- rt$durationUnit
      res <- as.data.frame(res)
    }
    res
  }
  if(verbose) message("\n",appendLF=FALSE)
  return( parse.json[[service]](j) )
}
