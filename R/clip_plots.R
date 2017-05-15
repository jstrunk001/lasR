clip_plots=function(

  idxyd=NA #id,x,y,diameter
  ,idxy=NA #id,xy coordinates of vertices for each polygon
  ,plot_polys=NA #shapefile path, or sp object for plots
  ,lasR_project=NA
  ,lasR_project_polys=NA
  ,dir_out=NA
  ,height=F
  ,do_plot=F
  ,return=F
  ,n_core=6
  ,dir_dtm=NA #in case drive paths are wrong (External drives...)
  ,dir_las=NA #in case drive paths are wrong (External drives...)

){
  require(rgdal)
  require(plyr)
  require(rgdal)
  require(rgeos)
  require(raster)
  require(lidR)
  require(data.table)
  require(parallel)

  #load lasR_project
  if(!is.na(lasR_project) & is.na(lasR_project_polys[1])){
    if(!inherits(lasR_project,"sp")){
      proj=read.csv(lasR_project)
      proj_polys0=bbox2polys(proj[,c("tile_id","mnx","mxx","mny","mxy")])
      row.names(proj)=proj[,"tile_id"]
      proj_polys=SpatialPolygonsDataFrame(proj_polys0,proj)
    }
    if(inherits(lasR_project,"sp")) proj_polys=lasR_project
  }
  if(!is.na(lasR_project_polys[1])){
    if(!inherits(lasR_project_polys,"sp")) proj_polys=readOGR(dirname(lasR_project_polys),basename(lasR_project_polys))
    if(inherits(lasR_project_polys,"sp")) proj_polys=lasR_project_polys
  }
print("load lasR_project");print(Sys.time())
  #fix drive paths in lasR_project
  if(!is.na(dir_dtm)) proj_polys@data[,"dtm_file"]=unlist(lapply(proj_polys@data[,"dtm_file"],function(...,dir_dtm)paste(file.path(dir_dtm,basename(strsplit(...,",")[[1]])),collapse=","),dir_dtm=dir_dtm))
  if(!is.na(dir_las)) proj_polys@data[,"las_file"]=unlist(lapply(proj_polys@data[,"las_file"],function(...,dir_dtm)paste(file.path(dir_dtm,basename(strsplit(...,",")[[1]])),collapse=","),dir_dtm=dir_las))

  #create sp objects for plots
  polys_in=NULL
  if(!is.na(plot_polys[1])){
    if(inherits(plot_polys,"sp")) polys_in=plot_polys
    if(inherits(plot_polys,"char")) polys_in=readOGR(plot_polys)
  }
  if(!is.na(idxy[1]) & !inherits(polys_in,"sp")){
    polys_in=points2polys(idxy)
  }
  if(!is.na(idxyd[1]) & !inherits(polys_in,"sp")){

    seq1=c(seq(-pi,pi,.1),pi+.1)
    circle1=data.frame(sin(seq1),cos(seq1))
    spl_idxyd=split(idxyd,1:nrow(idxyd))
    idxy=rbind.fill(lapply(spl_idxyd,function(x,circle1)data.frame(id=x[,1],x=circle1[,1]*x[,4]+x[,2],y=circle1[,2]*x[,4]+x[,3]),circle1))
    row.names(idxyd)=idxyd[,1]

    plot_polys_0=points2polys(idxy)
    plot_polys=SpatialPolygonsDataFrame(plot_polys_0,data=idxyd)

  }
print("Create Plot Polys");print(Sys.time())

  #clean up self intersections
  proj_polys_b=gBuffer(proj_polys, byid=TRUE, width=0)
  plot_polys_b=gBuffer(plot_polys, byid=TRUE, width=0)

  #clip data to match extents
  ext_tile=as(extent(as.vector(t(bbox(proj_polys_b)))), "SpatialPolygons")
  ext_plot=as(extent(as.vector(t(bbox(plot_polys_b)))), "SpatialPolygons")
  plot_polys_ext=gIntersection(plot_polys_b,ext_tile, byid=T,drop_lower_td=T)
  proj_polys_ext=gIntersection(proj_polys_b,ext_plot, byid=T,drop_lower_td=T)

  #patch data back onto clipped polygons
  row.names(proj_polys_ext)=gsub(" 1","",row.names(proj_polys_ext))
  row.names(plot_polys_ext)=gsub(" 1","",row.names(plot_polys_ext))
  keep_proj=row.names(proj_polys_b@data) %in% row.names(proj_polys_ext)
  keep_plot=row.names(plot_polys_b@data) %in% row.names(plot_polys_ext)
  proj_polys_b1=proj_polys_b[keep_proj,]
  plot_polys_b1=plot_polys_b[keep_plot,]

  plot_polys_spdf=SpatialPolygonsDataFrame(plot_polys_ext,plot_polys_b1@data)
  proj_polys_spdf=SpatialPolygonsDataFrame(proj_polys_ext,proj_polys_b1@data)

print("clip, recombine plots with data");print(Sys.time())

  #intersect plots with tiles
  proj_plot_x=gIntersects(plot_polys_spdf,proj_polys_spdf, byid=T,returnDense=F)
  proj_plot_x1=proj_plot_x[sapply(proj_plot_x,function(x)length(x)>0)]

print("intersect plots and tiles");print(Sys.time())

  #parse intersections and compile data
  plots_tiles=rbind.fill(mapply(function(x,y,plots,tiles){data.frame(plots[as.character(x),],tiles[y,],row.names=NULL)},names(proj_plot_x1),proj_plot_x1,SIMPLIFY = F, MoreArgs = list(plots=plot_polys_spdf@data,tiles=proj_polys_spdf@data)))

  #merge duplicate records
  dup_id=.dup2(plots_tiles$plot)
  dups=plots_tiles[dup_id,]
  no_dups_df=plots_tiles[!dup_id,]
  spl_dups=split(dups,dups$plot)
  dups_df=.fn_merge(spl_dups)
  plots_tiles_unq=rbind(no_dups_df,dups_df)
  row.names(plots_tiles_unq)=plots_tiles_unq[,1]

print("merge duplicates");print(Sys.time())
browser()
  #add records to geometry
  plot_polys_merge=SpatialPolygonsDataFrame(plot_polys_ext[names(plot_polys_ext) %in% (plots_tiles_unq[,1])],plots_tiles_unq)

  if(do_plot){
    plot(proj_polys_spdf)
    plot(plot_polys_merge,border="red",add=T,lwd=10)
  }

  #clip points
  spl_plots=split(plot_polys_merge,1:nrow(plot_polys_merge))

  if(n_core>1){
    clus=makeCluster(n_core)
    parLapply(clus,spl_plots,.try_clip_plots,dir_out = dir_out,height=height)
    stopCluster(clus)
  }
  if(n_core<2){
    lapply(spl_plots,.try_clip_plots,dir_out = dir_out,height=height)
  }

  #.try_clip_plots(spl_plots[[2]],dir_out = dir_out)
print("clip plots");print(Sys.time())

  #write shapefile of intersections
  dir_overlap=file.path(dir_out,"plot_tile_overlap")
  if(!dir.exists(dir_overlap)) dir.create(dir_overlap)
  writeOGR(plot_polys_merge,dir_overlap,"plot_merge_tiles", driver="ESRI Shapefile")

print("write outputs");print(Sys.time())

  if(return) return(plot_polys_merge)

}
.dup2=function(x,type=c("all","first","last"),value=F,invert=F){

  if(type[1]=="all") index = duplicated(x) | duplicated(x, fromLast=TRUE)
  if(type[1]=="first") index = duplicated(x)
  if(type[1]=="last") index = duplicated(x, fromLast=TRUE)

  if(invert) index = !index

  if(value) return(x[index])
  if(!value) return(index)

}

.fn_merge=function(x){
  require(data.table)
  if(class(x)=="list") x_in=data.frame(rbindlist(lapply(x,.fn_merge)))
  else{
    x_in=x[1,]
    x_in[,"tile_id"]=paste(x[,"tile_id"],collapse=",")
    x_in[,"las_file"]=paste(unique(strsplit(paste(x[,"las_file"],collapse=","),",")[[1]]),collapse=",")
    x_in[,"dtm_file"]=paste(unique(strsplit(paste(x[,"dtm_file"],collapse=","),",")[[1]]),collapse=",")
    if("x.1" %in% names(x_in)) x_in[,"x.1"]=paste(x[,"x.1"],collapse=",")
    if("y.1" %in% names(x_in)) x_in[,"y.1"]=paste(x[,"y.1"],collapse=",")
    x_in[,"mnx"]=min(x[,"mnx"])
    x_in[,"mxx"]=max(x[,"mxx"])
    x_in[,"mny"]=min(x[,"mny"])
    x_in[,"mxy"]=max(x[,"mxy"])
  }
  return(x_in)
}

.try_clip_plots=function(...){

  require(lasR)

  .clip_plots=function(x,dir_out,return=F,height=T){

    require(lidR)
    require(lasR)

    poly_coords=x@polygons[[1]]@Polygons[[1]]@coords

    las_in=readLAS(files=unlist(strsplit(x@data[,"las_file"],",")[1]))
    dtm_in=read_dtm(unlist(strsplit(x@data[,"dtm_file"],",")[1]))
    dtm_poly=try(crop(dtm_in,x))
    if(class(dtm_poly)=="try-error"){warning("plot and dem do not intersect, plot: ",x@data[,1]);return()}
    las_poly=lasclip(las_in, "polygon", poly_coords , inside = TRUE)
    if(height) las_hts = lasnormalize(las_poly, dtm = dtm_poly)
    if(!height) las_hts = las_poly
    las_hts@header@data['X scale factor'] = 0.001
    las_hts@header@data['Y scale factor'] = 0.001
    las_hts@header@data['Z scale factor'] = 0.001
    las_hts@header@data['X offset']=1
    las_hts@header@data['Y offset']=1
    las_hts@header@data['Z offset']=1

    #write to file
    if(!dir.exists(dir_out))dir.create(dir_out)
    out_file_i=file.path(dir_out,paste(names(x)[1],"_",x@data[,1],".las",sep=""))
    writeLAS(las_hts,out_file_i)

    # writeLAS(las_poly,"C:\\R\\analyses\\lidR_bug\\sample_elevation.las")
    # writeLAS(las_poly,"C:\\R\\analyses\\lidR_bug\\sample_height.las")
    # writeRaster(dtm_poly,"C:\\R\\analyses\\lidR_bug\\sample_dtm.tif")

  }

  try(.clip_plots(...))

}
#
# .clip_plots=function(x,dir_out,return=F){
#
#   require(lidR)
#   require(lasR)
#
#   #browser()
#
#   poly_coords=x@polygons[[1]]@Polygons[[1]]@coords
#
#   las_in=readLAS(files=unlist(strsplit(x@data[,"las_file"],",")[1]))
#   dtm_in=read_dtm(unlist(strsplit(x@data[,"dtm_file"],",")[1]))
#   dtm_poly=try(crop(dtm_in,x))
#   if(class(dtm_poly)=="try-error"){warning("plot and dem do not intersect, plot: ",x@data[,1]);return()}
#   las_poly=lasclip(las_in, "polygon", poly_coords , inside = TRUE)
#   las_hts=lasnormalize(las_poly, dtm = dtm_poly)
#
#
#   #write to file
#   if(!dir.exists(dir_out))dir.create(dir_out)
#   out_file_i=file.path(dir_out,paste(names(x)[1],"_",x@data[,1],".las",sep=""))
#   writeLAS(las_hts,out_file_i)
#
# }

if(F){

  library(rgdal)
  library(lasR)
  plxy=readOGR("C:\\projects\\2017_WA_DSM_Pilot\\plots","fia_plots_fuzz")
  proj4string(plxy)
  projTo="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  plxy1=spTransform(plxy,projTo)
  plxy1@data[,c("x","y")]=coordinates(plxy1)
  idxyd1=data.frame(plxy1@data[,c("PLOT","x","y")],d=24)
  names(idxyd1)=tolower(names(idxyd1))
  idxyd2=idxyd1[!duplicated(idxyd1$plot),]

  clip_plots(lasR_project="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\lasR_project001.csv"
             ,idxyd=idxyd2
             ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\plot_clips"
             #fix bad directory:
             ,dir_las="I:\\phodar\\NAIP_2015\\las_files\\"
             #fix bad directory:
             ,dir_dtm="H:\\DNR\\FUSION_DTMS\\"
             ,n_core=8

  )

  clip_plots(lasR_project="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\lasR_project001.csv"
             ,idxyd=idxyd2
             ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\plot_clips_elev"
             #fix bad directory:
             ,dir_las="I:\\phodar\\NAIP_2015\\las_files\\"
             #fix bad directory:
             ,dir_dtm="H:\\DNR\\FUSION_DTMS\\"
             ,n_core=7
             ,height=F

  )

}
