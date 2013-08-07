\name{meuse.grid}
\alias{meuse.grid}
\docType{data}
\title{ Prediction Grid for Meuse Data Set }
\usage{data(meuse.grid)}
\description{
The meuse.grid has a grid with 0.0002 degrees x 0.0002 degrees 
spacing that covers the Meuse study area (see \link{meuse}) in a 
latitude longitude format . 

}
\format{
  This data frame contains the following columns:
  \describe{
    \item{x}{a numeric vector; latitude coordinate (see \link{meuse}) }
    \item{y}{a numeric vector; longitude coordinate (see \link{meuse}) }
  }
}
\details{
\code{x} and \code{y} are in latitude and longitude format. Using +datum=WGS84.
}
\source{
\url{http://www.gstat.org/}
}
\references{
See the \link{meuse} documentation
}
\examples{
data(meuse.grid)
\dontrun{
spRplot(meuse.grid, col.pallete=list(col='lightgrey',alpha=0.5),maptype = 'hybrid')
}
}
\keyword{datasets}
