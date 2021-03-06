% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spRplot.R
\name{spRplot}
\alias{spRplot}
\title{Interface GoogleMaps within R}
\usage{
spRplot(data, var = NULL, maptype = "satellite", decimals = 3,
  add = FALSE, cuts = 5, cuts.type = "range", col.pallete = list(col =
  heat.colors(if (length(cuts) == 1) cuts else length(cuts)), alpha = 1),
  legend.att = NULL, zoom = NULL, lwd = 2, ...)
}
\arguments{
\item{data}{a spatial data from any sp classes.}

\item{var}{an attribute name in the spatial data.}

\item{maptype}{GoogleMaps types: 'satellite', 'terrain', 'roadmap', 'satellite', 'hybrid'.}

\item{decimals}{number of decimals in the legend.}

\item{add}{boolean. Default FALSE, if adding information to a previous plot.}

\item{cuts}{number of cuts in the legend. Can also be a vector with the cuts point.}

\item{cuts.type}{method of selecting the cuts points. Ignored if cuts = vector .}

\item{col.pallete}{a list containing the pallete of colors to be used (col) and the 
transparency level (alpha) .}

\item{legend.att}{a list containing the attributes for legend. See \code{\link[graphics]{legend}}.}

\item{zoom}{zoom level.}

\item{lwd}{the line width for SpatialLines/SpatialLinesDataframe class.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
A plot window on the browser.
}
\description{
Plot spatial data with GoogleMaps background.
}
\examples{

\dontrun{
spRplot(rio)
}
}
\seealso{
See \code{\link{spGplot}}.
}

