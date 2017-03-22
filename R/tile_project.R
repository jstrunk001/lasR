tile_project=function(

  folder_dtms
  ,folder_las
  ,project_folder="c:/lidar_projects/"
  ,project_name="test_project"
  ,inventory_dtms=T
  ,inventory_las=T
  ,create_project=T
  ,tile_size=1650
  ,pixel_size=66
  ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066,resolution=1650

){

  require("DBI")
  require("RSQLite")

  #test for what already exists
    #sqlite database
    #las inventory
    #dtm inventory

  #inventory las and dtms


  #create processing and sub-processing tiles


  #intersect tiles with polygons






}


# install.packages("raster")
# install.packages("RPostgreSQL")
# install.packages("lasR")
# install.packages("rgeos")
# install.packages("raster")
# install.packages("plyr")
# install.packages("data.table")
# install.packages("zoom")

library(raster)
library(RPostgreSQL)
library(lasR)
library(rgeos)
library(raster)
library(plyr)
library(data.table)
library(zoom)

#connect to database
if(!"con_inv" %in% ls()){

  con_inv=dbConnect(dbDriver("PostgreSQL"),dbname="inventory",host="localhost",port="5432",user="postgres",password="0000")
}

#scan las file
scan_las(project="naip", project_year="2015",dir_las="F:/phodar/NAIP_2015/las_files",con=con_inv)

#scan dtm files
scan_dtm(project="naip", project_year="2015",dir_dtm="E:\\FUSION_DTMS\\",con=con_inv,notes="Composite DEM over WA State")

#read in dtms
dtm_polys=readRDS(paste("f:\\DNR\\FUSION_DTMS\\manage_dtm\\dtm_polys.RDS",sep=""))
dtm_polys1=buffer(dtm_polys,66*2+1,dissolve=F)
# dtm_polys2=gSimplify(dtm_polys1,tol=10)

#read in lidar
las_polys=readRDS(paste("g:\\phodar\\NAIP_2015\\las_files\\manage_las\\las_polys.RDS",sep=""))
las_polys1=buffer(las_polys,2*66+1,dissolve=F)
# las_polys2=gSimplify(las_polys1,tol=10)
# las_polys2@data=las_polys@data

#create processing tiles
#create super tile
proc_rast=raster(xmn=561066,xmx=2805066,ymn=33066,ymx=1551066,resolution=1650
                 ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
)
proc_rast[]=cellsFromExtent(proc_rast,extent(proc_rast))

#create mini tiles and populate with super-tile ids
proc_rast1=raster(xmn=561066,xmx=2805066,ymn=33066,ymx=1551066,resolution=1650/10
                  ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
)
xy=as.data.frame(proc_rast1,xy=T)
xy[,"layer"]=NULL
xy[,"tile_id"]=cellFromXY(proc_rast, xy[,c(1:2)])
proc_rast1[]=xy[,"tile_id"]
proc_rast1[]=xy[,"tile_id"]
proc_rast2=rasterFromXYZ(xy[,c("x","y","tile_id")],res=)

saveRDS(proc_rast,"C:\\R\\analyses\\process_gridmetrics\\data\\proc_rast.rds")
saveRDS(proc_rast1,"C:\\R\\analyses\\process_gridmetrics\\data\\proc_rast1.rds")
saveRDS(proc_rast2,"C:\\R\\analyses\\process_gridmetrics\\data\\proc_rast2.rds")

#intersect polygons with processing tiles

proc_rast2=readRDS("C:\\R\\analyses\\process_gridmetrics\\data\\proc_rast2.rds")

ex_dtm=extract(proc_rast2,dtm_polys1);gc()
ex_dtm1=lapply(ex_dtm,unique);gc()
names(ex_dtm1)=dtm_polys1$file_path
ex_las=extract(proc_rast2,las_polys1);gc()
ex_las1=lapply(ex_las,unique);gc()
names(ex_las1)=las_polys1$file_path

saveRDS(ex_las1,"C:\\R\\analyses\\process_gridmetrics\\data\\ex_las1.rds")
saveRDS(ex_dtm1,"C:\\R\\analyses\\process_gridmetrics\\data\\ex_dtm1.rds")


#create dataframe from intersections
tiles_las_df=data.frame(rbindlist(mapply(function(tile_id,file){data.frame(tile_id,las_file=file,stringsAsFactors=F)},ex_las1,names(ex_las1),SIMPLIFY=F)))
sum(duplicated(tiles_las_df[,"las_file"]))

tiles_dtm_df=data.frame(rbindlist(mapply(function(tile_id,file){data.frame(tile_id,dtm_file=file,stringsAsFactors=F)},ex_dtm1,names(ex_dtm1),SIMPLIFY=F)))
sum(duplicated(tiles_dtm_df[,"dtm_file"]))

tiles_dtm_agg=aggregate(dtm_file~tile_id,data=tiles_dtm_df,FUN=function(x)paste(unique(x),collapse=","))
tiles_las_agg=aggregate(las_file~tile_id,data=tiles_las_df,FUN=function(x)paste(unique(x),collapse=","))

tiles_las_dtm=merge(tiles_las_agg,tiles_dtm_agg,by="tile_id")


saveRDS(tiles_las_dtm,"C:\\R\\analyses\\process_gridmetrics\\data\\tiles_las_dtm.rds")











#test overlapp
las_polys1=buffer(las_polys,2*66+1,dissolve=F)
proc_test=proc_rast1

r1=crop(proc_test,las_polys1[10:15,])
r1[]=sample(1:33)
plot(r1)
plot(las_polys1[10:15,],add=T)

r2=crop(proc_test,las_polys1[10:15,])
test_tiles=cellFromPolygon(r2, las_polys1[10:15,], weights=FALSE)

cellFromPolygon(r2, las_polys1[10:15,], weights=FALSE)
extract(r2, las_polys1[10:15,],cellnumbers=F)

extract(r1,las_polys[5:7,])
extract(r1,las_polys1[5:7,])
extract(r1,las_polys1[5:7,],sp=T)

extract(r1,las_polys[5:6,],small=F, df=T,cellnumbers=T,weights=T, buffer=1000000)


plot(r1)
plot(las_polys[1:50,],add=T)
plot(las_polys1[1:50,],add=T)


plot(las_polys[5:7,])
plot(r1,add=T)
plot(las_polys1[5:7,],add=T)
plot(las_polys[5:7,],add=T)


#ex_test=
system.time(extract(proc_rast1,dtm_polys2[1:5,]))
extract(proc_rast1,dtm_polys2[1:5,],fun=unique,along=F)

r3=crop(proc_rast2,dtm_polys2[1:5,])

plot(dtm_polys2[1:50,],add=T)

r4=crop(proc_rast,dtm_polys2[1:5,])

plot(r4,add=T)
plot(r3,add=T)
#ex_test1=
system.time(cellFromPolygon(proc_rast1,dtm_polys2[1:5,]))



# experiment
proc_poly=rasterToPolygons(proc_rast, fun=NULL, n=4, na.rm=TRUE, digits=2, dissolve=FALSE)
saveRDS(proc_poly,"C:\\R\\analyses\\process_gridmetrics\\data\\proc_poly.rds")

proj4string(proc_poly)=proj4string(dtm_polys)
test=gIntersection(proc_poly,dtm_polys,byid=T)


test=as.data.frame(rasterToPoints(proc_test))

plot(dtm_polys[100,])
text(test)

#create inference grid - e.g. pixels
pix_rast=raster( xmn=561066,xmx=2805066,ymn=33066,ymx=1551066,resolution=66
                 ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
)
saveRDS(pix_rast,"C:\\R\\analyses\\process_gridmetrics\\data\\pix_rast.rds")

