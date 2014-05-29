\name{interpolateWithinSingleSpatial}
\alias{interpolateWithinSingleSpatial}
\title{Interpolate and sample within a single polygon
(Called by interpolateWithinSpatial)}
\usage{
  interpolateWithinSingleSpatial(crudeSingle, fineWithin,
    FUN, nSampleCol, samplesize, simplify = FALSE, ...)
}
\arguments{
  \item{crudeSingle}{A single polygon}

  \item{fineWithin}{A points or polygon sp object whose
  elements lie within crudeSingle}

  \item{FUN}{A function to be applied to them after
  sampling}

  \item{nSampleCol}{Column name containing number of
  elements of fineWithin to sample}

  \item{samplesize}{The number of replicates per element of
  crude to draw}

  \item{simplify}{Whether to simplify to an array or not}

  \item{\dots}{Arguments to FUN}
}
\value{
  List of FUN's results for each sampling
}
\description{
  Interpolate and sample within a single polygon (Called by
  interpolateWithinSpatial)
}

