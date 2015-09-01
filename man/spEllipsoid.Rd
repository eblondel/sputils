\name{spEllipsoid}

\docType{methods}

\alias{spEllipsoid}
\alias{spEllipsoid,ellipsoid-method}
\alias{spEllipsoid,SpatialPoints-method}


\title{Create an ellipsoid as SpatialPolygons object}

\description{
Methods to create an ellipsoid as SpatialPolygons object. Spatial ellipsoids can be created from a \code{\link[cluster]{ellipsoid}} or \code{\link[sp]{SpatialPoints}} object.
}

\usage{

\S4method{spEllipsoid}{ellipsoid}(x, proj4string = NA)

\S4method{spEllipsoid}{SpatialPoints}(x, key = NULL, value = NULL)

}

\arguments{
\item{x}{ellipsoid or SpatialPoints* object.}
\item{proj4string}{CRS object}
\item{key}{a key representing a column name if x is a SpatialPointsDataFrame object}
\item{value}{a value repsententing a column filter value, if x is a SpatialPointsDataFrame object}
}

\value{
SpatialPolygons
}

\author{Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}}

\seealso{
\code{\link[sp]{SpatialPolygons}},
\code{\link[cluster]{ellipsoid}}
}

\examples{

#sample dataset
data(meuse)
coordinates(meuse) <- c("x","y")

#create a spatial ellipsoid
ell <- spEllipsoid(meuse)

#plot
plot(meuse)
plot(ell, add = TRUE, border = "red")

}

\keyword{methods}
\keyword{spatial}
\keyword{ellipsoid}