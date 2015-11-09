\name{rio}
\alias{rio}
\docType{data}
\title{ Dengue Fever in the State of Rio de Janeiro }
\description{
  Shape file of the state of Rio de Janeiro
}
\usage{data(rio)}
\format{
  A shape file with 92 observations on the following 9 variables.
  \describe{
    \item{GeoCode}{Rio de Janeiro counties numeric codes}
    \item{Name}{Rio de Janeiro counties names}
    \item{Lat}{Rio de Janeiro counties latitude}
    \item{Long}{Rio de Janeiro counties longitude}
    \item{Pop}{Rio de Janeiro counties population}
    \item{Income}{Rio de Janeiro counties percentage of homes living with less than one minimun salary}
    \item{Urban}{Rio de Janeiro counties percentage of urban area}
    \item{Dengue}{Counts of dengue fever cases by county in the state of Rio de Janeiro}
    \item{E}{Expected counts of dengue fever cases by county in the state of Rio de Janeiro, conditioned on the total
             number of cases the stratified by population rate}
    \item{SMR}{The standard mortality ratio, calculated as the observed/expected counts}
  }
}

\examples{
data(rio)

\dontrun{
##Using spRplot
#plot the shapefile
spRplot(rio, col.pallete=list(col='green',alpha=0.5))

#plot the characteristcs of data
spRplot(rio,'SMR',cuts=c(0,.5,1,2,5,10),col.pallete=list(alpha=0.75))
spRplot(rio,'Income',cuts=c(0,.2,.4,.6,.8,1))
spRplot(rio,'Urban',cuts=c(0,.2,.4,.6,.8,1))
              
##Using spGplot
#spGplot(rio, 
        var = 'SMR', 
        description = list(var = c('Income', 'Urban'), type = 'bar', title = 'Name'), 
	      cuts=c(0,.5,1,2,5,10), 
        col.pallete = list(alpha=0.8))
}
}
\keyword{datasets}
