\name{georoute}
\alias{georoute}
\alias{georoute.default}
\title{Find driving routes using online services}
\usage{
  georoute(x, verbose = FALSE, service = "bing",
    returntype = "all", ...)

  \method{georoute}{default} (x, verbose = FALSE,
    service = "bing", returntype = "all", ...)
}
\arguments{
  \item{x}{A character vector of length>=2, where each
  element is a (starting, ending, or intermediate)
  waypoint, or a numeric matrix with columns c('lat','lon')
  where each row is a waypoint}

  \item{verbose}{Provide additional information}

  \item{returntype}{What information to return.  Currently,
  the options are "all", "distance", "distanceUnit",
  "path", "time", and/or "timeUnit".  Can be combined, as
  in returntype=c("time","distance").}

  \item{service}{API to use.  Currently the only option is
  "bing"}

  \item{\dots}{Other items to pass along}
}
\value{
  Route information (see the returntype argument)
}
\description{
  Find transit routes using Google or Bing's API
}
\examples{
\dontrun{
georoute( c("3817 Spruce St, Philadelphia, PA 19104", "9000 Rockville Pike, Bethesda, Maryland 20892"), verbose=TRUE )
georoute( c("3817 Spruce St, Philadelphia, PA 19104", "Tulum, MX","9000 Rockville Pike, Bethesda, Maryland 20892"), returntype="distance" )
georoute( c("3817 Spruce St, Philadelphia, PA 19104", "9000 Rockville Pike, Bethesda, Maryland 20892"), verbose=TRUE, returntype="path" )
georoute( c("3817 Spruce St, Philadelphia, PA 19104", "9000 Rockville Pike, Bethesda, Maryland 20892"), verbose=TRUE, returntype="time" )
# Using lat/lon
xmat <- rbind( geocode( "3817 Spruce St, Philadelphia, PA 19104" ), geocode( "9000 Rockville Pike, Bethesda, Maryland 20892" ) )
colnames(xmat) <- c( 'lat', 'lon' )
georoute( xmat, verbose=TRUE, returntype = c("distance","distanceUnit") )
}
}
\author{
  Ari B. Friedman
}

