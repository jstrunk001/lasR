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
#'@param idxxyy dataframe with 5 columns - id, minx,maxx, miny, maxy - representing the id, x, and y of vertices of polygons
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@importFrom plyr rbind.fill
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#'@export
bbox2polys=function(idxxyy){



  idxy_in=plyr::rbind.fill(
    apply(idxxyy,1,function(x) data.frame(id=x[1],x=x[c(2,2,3,3,2)],y=x[c(4,5,5,4,4)],row.names=NULL))
  )

  points2polys(idxy_in)

}

