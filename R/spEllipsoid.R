# spEllipsoid.R
# -------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2015-09-01
# Version 0.1
# Licence GPL v3

if ( !isGeneric("spEllipsoid") ) {
  setGeneric("spEllipsoid", function(x, ...)
    standardGeneric("spEllipsoid"))
}


spEllipsoid.ellipsoid <- function(x, proj4string = NA){
  
  pred <- predict(x)
  pred <- rbind(pred, pred[1,])
  pol <- Polygon(pred, hole=as.logical(NA))
  pols <- Polygons(srl = list(pol) , ID = 1)
  sp.ell <- SpatialPolygons(Srl = list(pols), proj4string = proj4string)

  return(sp.ell)

}


spEllipsoid.spatial <- function(x, key = NULL, value = NULL){
  
  cl <- x
  if(!is.null(key) && !is.null(value) && class(x) == "SpatialPointsDataFrame"){
    cl <- x[x@data[,key] == value,]
  }
  ell <- ellipsoidhull(coordinates(cl))
  sp.ell <- spEllipsoid.ellipsoid(ell, proj4string = CRS(proj4string(cl)))
  
  return(sp.ell)
}

setOldClass("ellipsoid")
setMethod('spEllipsoid', signature(x='ellipsoid'), spEllipsoid.ellipsoid)
setMethod('spEllipsoid', signature(x='SpatialPoints'), spEllipsoid.spatial)