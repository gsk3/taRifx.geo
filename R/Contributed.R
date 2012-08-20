### Various contributed functions


#'Geocode character vectors using online services
#'
#'Geocode character vectors (or data.frames) using Google or Bing's API
#'
#'
#'@aliases geocode geocode.default geocode.data.frame
#'@param x A vector or data.frame
#'@param verbose Whether to display each address as it is submitted to Google or not
#'@param addresscol A (character) name of the column in a data.frame which contains the addresses
#'@param service API to use.  Current options are "bing" or "google"
#'@param \dots Other items to pass along
#'@return geocode.default returns a numeric vector of length 2 containing the
#'latitudes and longitudes. geocode.data.frame returns the original data.frame
#'with two additional columns for the longitude and latitudes.
#'@author Basic REST algorithm by Tony Breyal (http://stackoverflow.com/a/3259537/636656). Error handling, object orientation, and Bing
#' capabilities by Ari Friedman
#'@examples
#'geocode("3817 Spruce St, Philadelphia, PA 19104")
#'geocode("Philadelphia, PA")
#'dat <- data.frame(value=runif(3),address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland"))
#'geocode(dat)
#'@rdname geocode
#'@export geocode
geocode <- function( x, verbose=FALSE, service="google", ... ) {
  require(RCurl)
  require(RJSONIO)
  UseMethod("geocode",x)
}
#'@rdname geocode
#'@method geocode default
#'@S3method geocode default
geocode.default <- function(x,verbose=FALSE, service="google", ...) {
  # Input regularization and checking
  service <- tolower(service)
  BingMapsKey <- getOption("BingMapsKey")
  if(service=="bing" && is.null(BingMapsKey) ) stop("To use Bing, must save your Bing Maps API key (obtain at http://msdn.microsoft.com/en-us/library/ff428642.aspx) using options(BingMapsKey='mykey').\n")
  # URL constructing
  construct.geocode.url <- list()
  construct.geocode.url[["google"]] <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))  
  }
  construct.geocode.url[["bing"]] <- function(address, maxResults=1) {
    root <- "http://dev.virtualearth.net/REST/v1/Locations"
    u <- paste0(root, "?query=", address, "&maxResults=",maxResults,"&key=",BingMapsKey)
    return(URLencode(u))
  }
  if(verbose) cat(x)
  u <- construct.geocode.url[[service]](x)
  doc <- getURL(u)
  j <- fromJSON(doc,simplify = FALSE)
  # Parse and return
  parse.json <- list()
  parse.json[["google"]] <- function(j) {
    if(j$status=="OK") {
      lat <- j$results[[1]]$geometry$location$lat
      lng <- j$results[[1]]$geometry$location$lng
      return(c(lat, lng))
    } else {
      if(j$status=="OVER_QUERY_LIMIT") warning("Google's geocoding quota appears to have been reached for the day.")
      return(c(NA,NA))
    }
  }
  parse.json[["bing"]] <- function(j) {
    if(j$authenticationResultCode != "ValidCredentials") stop("Your BingMapsKey was not accepted.\n")
    if(j$statusDescription!="OK") stop("Something went wrong. Bing Maps API return status code ",j$statusCode," - ", j$statusDescription,"\n")
    if(verbose) cat(" - Confidence: ", j$resourceSets[[1]]$resources[[1]]$confidence)
    unlist(j$resourceSets[[1]]$resources[[1]]$point$coordinates)
  }
  res <- parse.json[[service]](j)
  if(verbose) cat("\n")
  return( res )
}
#'@rdname geocode
#'@method geocode data.frame
#'@S3method geocode data.frame
geocode.data.frame <- function(x, verbose=FALSE, service="google", addresscol="address", ...) {
  latlon <- as.data.frame(t(sapply(x[[addresscol]],geocode,verbose=verbose,service=service)))
  colnames(latlon) <- c("lat","lon")
  cbind( x, latlon )
}

#'Geocode using Google Maps API (deprecated)
#'This has been generalized to the taRifx::geocode() function
#'@param \dots Ignored
#'@export gGeoCode
gGeoCode <- function(...) {
  .Deprecated("geocode","taRifx","gGeoCode has been generalized to other services.  Enjoy!")
}