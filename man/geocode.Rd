\name{geocode}
\alias{geocode}
\alias{geocode.data.frame}
\alias{geocode.default}
\title{Geocode character vectors using online services}
\usage{
  geocode(x, verbose = FALSE, service = "google",
    returntype = "coordinates", ...)

  \method{geocode}{default} (x, verbose = FALSE,
    service = "google", returntype = "coordinates", ...)

  \method{geocode}{data.frame} (x, verbose = FALSE,
    service = "google", returntype = "coordinates",
    addresscol = "address", ...)
}
\arguments{
  \item{x}{A vector or data.frame}

  \item{verbose}{Whether to display each address as it is
  submitted to Google or not}

  \item{service}{API to use.  Current options are "bing" or
  "google".  To add your Bing API key, set
  options(BingMapsKey="yourkeygoeshere").}

  \item{returntype}{What to return.  Options include
  "coordinates" and "zip".}

  \item{addresscol}{A (character) name of the column in a
  data.frame which contains the addresses}

  \item{\dots}{Other items to pass along}
}
\value{
  geocode.default returns a numeric vector of length 2
  containing the latitudes and longitudes.
  geocode.data.frame returns the original data.frame with
  two additional columns for the longitude and latitudes.
}
\description{
  Geocode character vectors (or data.frames) using Google
  or Bing's API
}
\examples{
\dontrun{
geocode("3817 Spruce St, Philadelphia, PA 19104")
geocode("Philadelphia, PA")
dat <- data.frame(value=runif(3),address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland"))
geocode(dat)
}
}
\author{
  Error handling, object orientation, and Bing capabilities
  by Ari Friedman. Google REST algorithm by Tony Breyal
  (http://stackoverflow.com/a/3259537/636656).
}

