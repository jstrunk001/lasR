#'@name points2polys
#'@title
#'  <Delete and Replace>
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param idxy dataframe with 3 columns - id, x, y - representing the id, x, and y of vertices of polygons
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import sp
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
#
points2polys=function(idxy){

  spl_idxy=split(idxy[,2:3], idxy[,1])

  polys <- SpatialPolygons(

    mapply(
      function(poly, id) {
        Polygons(list(Polygon(poly)), ID=id)
      }
      , spl_idxy
      , names(spl_idxy))

  )

  polys

}

