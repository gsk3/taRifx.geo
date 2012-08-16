### Various contributed functions


#'Geocode character vectors using Google's API
#'
#'Geocode character vectors (or data.frames) using Google's API
#'
#'
#'@aliases gGeoCode gGeoCode.default gGeoCode.data.frame
#'@param x A vector or data.frame
#'@param verbose Whether to display each address as it is submitted to Google or not
#'@param addresscol A (character) name of the column in a data.frame which contains the addresses
#'@param floodControl If TRUE, inserts a random delay between requests to be polite to the server
#'@param \dots Other items to pass along
#'@return gGeoCode.default returns a numeric vector of length 2 containing the
#'latitudes and longitudes. gGeoCode.data.frame returns the original data.frame
#'with two additional columns for the longitude and latitudes.
#'@author Tony Breyal (http://stackoverflow.com/a/3259537/636656), with error handling and object orientation by Ari
#'Friedman
#'@examples
#'gGeoCode("3817 Spruce St, Philadelphia, PA 19104")
#'gGeoCode("Philadelphia, PA")
#'dat <- data.frame(value=runif(3),address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland"))
#'gGeoCode(dat)
#'@rdname gGeoCode
#'@export gGeoCode
gGeoCode <- function( x, verbose=FALSE, floodControl=FALSE, ... ) {
  require(RCurl)
  require(RJSONIO)
  UseMethod("gGeoCode",x)
}
#'@rdname gGeoCode
#'@method gGeoCode default
#'@S3method gGeoCode default
gGeoCode.default <- function(x,verbose=FALSE, floodControl=FALSE, ...) {
  if(floodControl) Sys.sleep(runif(1)/10) # Flood control to be polite to the server
  construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))  
  }
  if(verbose) cat(x,"\n")
  u <- construct.geocode.url(x)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    if(x$status=="OVER_QUERY_LIMIT") warning("Google's geocoding quota appears to have been reached for the day.")
    return(c(NA,NA))
  }
}
#'@rdname gGeoCode
#'@method gGeoCode data.frame
#'@S3method gGeoCode data.frame
gGeoCode.data.frame <- function(x, verbose=FALSE, floodControl=FALSE, addresscol="address", ...) {
  latlon <- as.data.frame(t(sapply(x[[addresscol]],gGeoCode,verbose=verbose)))
  colnames(latlon) <- c("lat","lon")
  cbind( x, latlon )
}
