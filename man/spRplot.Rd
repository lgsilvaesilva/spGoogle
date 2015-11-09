\name{spRplot}
\alias{spRplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Interface GoogleMaps within R}
\description{
 Plot spatial data with GoogleMaps backgroud
}
\usage{
spRplot(data, var=NULL, maptype='satellite', decimals = 3, 
        add = FALSE,cuts = 5, cuts.type='range', 
        lwd = 1.5,
        col.pallete = list(col=heat.colors(if(length(cuts)==1) 
                           cuts else length(cuts)),alpha = 1)
        legend.position = 'bottomright')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a spatial data from any sp classes }
  \item{var}{ an attribute name in the spatial data }
  \item{maptype}{ GoogleMaps types: 'satellite', 'terrain', 'roadmap', 'satellite', 'hybrid'}
  \item{decimals}{ number of decimals in the legend }
  \item{add}{ boolean. Default FALSE, if adding information to a previous plot }
  \item{cuts}{ number of cuts in the legend. Can also be a vector with the cuts point }
  \item{cuts.type}{ method of selecting the cuts points. Ignored if cuts = vector }
  \item{col.pallete}{ a list containing the pallete of colors to be used (col) and the 
transparency level (alpha) }
  \item{lwd}{ the line width for SpatialLines/SpatialLinesDataframe class}
  \item{legend.position}{the x and y co-ordinates to be used to position the legend. They can be specified by keyword or in any way which is accepted by xy.coords}
}
\value{
  A plot window .
}
\seealso{
See \code{\link{rio}} and \code{\link{meuse}}
}
\examples{#see examples in 
?rio
?meuse}
\author{Luis Gustavo Silva e Silva \email{lgsilvaesilva@gmail.com} and Marcos Prates \email{marcosop@est.ufmg.br}}
\keyword{ interface }
