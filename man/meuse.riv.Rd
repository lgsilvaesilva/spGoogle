\name{meuse.riv}
\alias{meuse.riv}
\docType{data}
\title{ River Meuse outline }
\usage{data(meuse.riv)}
\description{
The meuse.riv data consists of an outline of the Meuse
river in the area a few kilometers around the \link{meuse}
data set. }
\format{
  This data frame polygon of the Meuse river in a latitude longitude format.
}

\references{
See the \link{meuse} documentation
}
\examples{
data(meuse.riv)
spRplot(meuse.riv,col.pallete=list(col='blue'))

}
\keyword{datasets}
