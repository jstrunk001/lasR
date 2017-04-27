clip_plots=function(
    idxyd=NA #id,x,y,diameter
    ,idxy=NA #id,xy coordinates of vertices for each polygon
    ,polys=NA #shapefile path, or sp object
    ,lasR_project=NA
    ,lasR_project_shp=NA
    ,dir_out
    ,height=F

  ){
  require(rgdal)

  if(!is.na(lasR_project) & is.na(lasR_project_shp)){
    proj=read.csv(lasR_project)
  }


  #create sp objects for plots
  polys_in=NULL
  if(!is.na(polys[1])){
    if(inherits(polys,"sp")) polys_in=polys
    if(inherits(polys,"char")) polys_in=readOGR(polys)
  }
  if(!is.na(idxy[1]) & !inherits(polys_in,"sp")){
    polys_in=points2polys(idxy)
  }
  if(!is.na(idxyd[1]) & !inherits(polys_in,"sp")){
browser()
    seq1=seq(-pi,pi,.1)
    circle1=c(sin(seq1),cos(seq1))
    spl_idxyd=split(idxyd,idxyd[,1])
    idxy=sapply(spl_idxyd,function(x,seq1)data.frame(x[1],circle1[,1]*x[4]+x[2],circle1[,2]*x[4]+x[3]),circle1)
    # seq1=seq(-pi,pi,.1)
    # plot(sin(seq1),cos(seq1))
    # sin(seq(-1,1)  )

    polys_in=points2polys(idxy)

  }

}

if(F){
  library(rgdal)
  plxy=readOGR("C:\\projects\\2017_WA_DSM_Pilot\\plots","fia_plots_fuzz")
  proj4string(plxy)
  projTo="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  plxy1=spTransform(plxy,projTo)
  plxy1@data[,c("x","y")]=coordinates(plxy1)
  idxyd1=data.frame(plxy1@data[,c("PLOT","x","y")],d=24)
  names(idxyd1)=tolower(names(idxyd1))

  clip_plots(lasR_project="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A_lasR\\intersections.csv",idxyd=idxyd1)

}
