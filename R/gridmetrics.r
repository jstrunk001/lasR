#'@title
#'  summarize lidar for raster pixels
#'
#'@description
#'  summarize lidar for raster pixels
#'
#'@details
#'  summarize lidar for raster pixels
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param las_files list of paths to las files
#'@param dtm_files optional list of paths to dtms
#'@param dtm_folder optional folder of dtms
#'@param dtm_ext what is the file extension of dtms
#'@param intersect_only only provide outputs over areas in which lidar and dtms intersect?
#'@param fns list of functions that will be used to generate columns of summary statistics from the las data
#'@param xmin (optional) force extent of analysis
#'@param xmax (optional) force extent of analysis
#'@param ymin (optional) force extent of analysis
#'@param ymax (optional) force extent of analysis
#'@param res resolution of processing analysis in units of provided lidar / dtm
#'@param grid pre-defined raster for analysis - lidar metrics are computed for each cell
#'
#'@return
#'  optional dataframe of xy coordinates for cell centers and additional columns of metrics
#'
#'@examples
#' dtms1="C:\\temp\\dtm_test\\"
#' las1="C:\\temp\\lidar_test\\"
#' las_files=list.files(las1,pattern="[.]las",full.names=T)
#' dtm_files=list.files(dtms1,pattern="[.]dtm",full.names=T)
#' metrics=gridmetrics(las_files=las_files,dtm_files=dtm_files)
#' head(metrics)
#'
#'@import raster, plyr
#'
#'@export
#'@seealso \code{\link{read_las}}\cr \code{\link{read_dtm}}\cr


gridmetrics=function(

  las_files
  ,dtm_files=NA
  ,dtm_folder=NA
  ,dtm_ext=".dtm"
  ,no_dtm=F
  ,fns=list(min=min,max=max,mean=mean,sd=sd,p20=function(x,...)quantile(x,.2,...))
  ,xmin=NA
  ,xmax=NA
  ,ymin=NA
  ,ymax=NA
  ,res=66

  ,grid=NA

  ,n_read=NA

  ,out_name=NA
  ,return=T
  ,con=NA
  ,out_table=""

){
  require(raster)
  require(plyr)

  #get / merge las data
  las_in=sapply(las_files,read_las,n_read=n_read,simplify=T)
  las_pts=rbind.fill(las_in["pts",])
  las_heads=rbind.fill(las_in["header",])
browser()
  #get / merge dtms
  if(!no_dtm){

    if(is.na(dtm_files[1]) & is.na(dtm_folder)) stop ("dtm paths or folders not provided")

    if(tolower(dtm_ext)==".dtm"){

      dtms=read_dtm(dtm_files,dtm_folder)

    }
    if(tolower(dtm_ext)!=".dtm"){

      if(is.na(dtm_files[1])) dtm_files=list.files(dtm_folder,pattern=dtm_ext,full.names=T)
      dtms=lapply(dtms,raster)

    }
print("mosaic dtms")
    #mosaic dtms
    if(class(dtms)=="list"){
      if(length(dtms)>1) mos_dtm=do.call(function(x,y,...,fun=max)mosaic(x,y,...,fun=max),dtms)
      else mos_dtm=dtms[[1]]
    }
    if(!class(dtms)=="list") mos_dtm=dtms
  }

  #difference points and rasters
  las_pts$be=extract(mos_dtm,las_pts[,c("X","Y")])
  las_pts$ht=las_pts$Z-las_pts$be
  coordinates(las_pts)=~X+Y

  #rasterize point data
print("intersect overlaps")

  #define overlap
  if(!no_dtm) overlap=intersect(extent(las_pts),extent(mos_dtm))
  if(no_dtm) overlap=extent(las_pts)
  if(!is.na(xmin)) overlap=intersect(overlap,extent(xmin,xmax, ymin, ymax))

  if(class(overlap)=="integer") {
    warning("nothing to process, too little or no intersection for ",las_files," ",dtm_files)
    return()
  }

  #create processing grid
  if(inherits(grid,"raster")) r0=grid
  if(!inherits(grid,"raster")){
    r0=raster(overlap,resolution=res)
  }
print("rasterize")
  br1=brick(lapply(fns,function(x,dat,rast,field)rasterize(x=dat,y=rast,fun=x,field=field),las_pts,r0,field="ht"))
print("zero pixels")
  #zero pixels without dem
  if(!no_dtm){

    br2=crop(br1,overlap);gc()
    mos_dtm1=crop(mos_dtm,overlap);gc()
    mos_dtm2=resample(mos_dtm1,br2);gc()
    #mos_dtm2=aggregate(mos_dtm1,br2);gc()
    br3=mask(br2,mos_dtm2);gc()
    br1=br3;gc()

  }

print("push to long format")
  #push to long format, write to csv
  xyz=as.data.frame(br1, xy=TRUE,na.rm=T)
  if(!is.na(out_tab)) dbWriteTable(con,out_table,xyz)
  if(!is.na(out_name)) write.csv(xyz,out_name,row.names=F)
  if(return) return(xyz)

}

