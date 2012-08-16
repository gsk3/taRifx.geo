\name{gGeoCode}
\alias{gGeoCode}
\alias{gGeoCode.data.frame}
\alias{gGeoCode.default}
\title{Geocode character vectors using Google's API}
\usage{
  gGeoCode(x, verbose = FALSE, floodControl = FALSE, ...)

  \method{gGeoCode}{default} (x, verbose = FALSE,
    floodControl = FALSE, ...)

  \method{gGeoCode}{data.frame} (x, verbose = FALSE,
    floodControl = FALSE, addresscol = "address", ...)
}
\arguments{
  \item{x}{A vector or data.frame}

  \item{verbose}{Whether to display each address as it is
  submitted to Google or not}

  \item{addresscol}{A (character) name of the column in a
  data.frame which contains the addresses}

  \item{floodControl}{If TRUE, inserts a random delay
  between requests to be polite to the server}

  \item{\dots}{Other items to pass along}
}
\value{
  gGeoCode.default returns a numeric vector of length 2
  containing the latitudes and longitudes.
  gGeoCode.data.frame returns the original data.frame with
  two additional columns for the longitude and latitudes.
}
\description{
  Geocode character vectors (or data.frames) using Google's
  API
}
\examples{
gGeoCode("3817 Spruce St, Philadelphia, PA 19104")
gGeoCode("Philadelphia, PA")
dat <- data.frame(value=runif(3),address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland"))
gGeoCode(dat)
}
\author{
  Tony Breyal (http://stackoverflow.com/a/3259537/636656),
  with error handling and object orientation by Ari
  Friedman
}

