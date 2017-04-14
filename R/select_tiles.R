#'@title
#'  subset tiles in a lasR project
#'
#'@description
#'  supply a polygon and subset tiles in a lasR project
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 April 14 Implemented \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'
#'@param path_project
#'@param path_shapefile
#'@param ignore_proj4
#'@param dir_out
#'@param return
#'@param plot

#'
#'@return
#'  NULL
#'
#'@examples
#'  tiles=select_tiles("C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\intersections.csv"
#'  ,"C:\\projects\\2017_WA_DSM_Pilot\\boundary\\extentB.shp"
#'  ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\subset_project\\"
#'  )
#'
#'@import rgdal,rgeos
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
#


select_tiles=function(
  path_project
  ,path_shapefile
  ,ignore_proj4=T
  ,dir_out=""
  ,return=F
  ,plot=T
  ){

  require(rgdal)
  require(rgeos)
  #load project
  proj_in=read.csv(path_project)

  #create spatial object from project
  tile_proj0=bbox2polys(proj_in[,c("tile_id","mnx","mxx","mny","mxy")])
  row.names(proj_in)=proj_in[,c("tile_id")]
  tile_proj1=SpatialPolygonsDataFrame(tile_proj0,proj_in)

  #load shapefile
  bounds=readOGR(dsn=dirname(path_shapefile),layer=gsub("[.]shp","",basename(path_shapefile)))

  #ignore proj strin
  proj4string(tile_proj1)=proj4string(bounds)

  #intersect tiles and bounds
  tiles_subset=gContains(bounds,tile_proj1,byid=T)
  if(sum(unlist(tiles_subset))>0) tile_proj2=tile_proj1[tiles_subset[,1,drop=T],]

  if(plot){
    plot(bounds)
    plot(tile_proj2,add=T)
  }

  writeOGR(tile_proj2, dsn=gsub("\\$","",backslash(dir_out)), layer="subset.shp", driver="ESRI Shapefile")
  write.csv(tile_proj2,paste(dir_out, "\\subset_intersections.csv",sep=""))
  }

if(F){

  tiles=select_tiles("C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\intersections.csv"
                     ,"C:\\projects\\2017_WA_DSM_Pilot\\boundary\\extentB.shp"
                     ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\subset_project\\"
                     )

  }
