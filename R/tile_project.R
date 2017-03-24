tile_project=function(

  dir_las
  ,dir_dtm
  ,project_folder="c:/lidar_projects/"
  ,project_name="test_project"
  ,project_year="2099"
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

  #create sqlite database / tables


  #inventory las and dtms
  scan_las(project=project_name, project_year="2015",dir_las=dir_las,con=con_proj)
  scan_dtm(project=project_name, project_year="2015",dir_dtm=dir_dtm,con=con_proj)


  #create processing and sub-processing tiles


  #intersect tiles with polygons


  #create dataframe from dtm and las intersections on tiles


  #save everything to sqlite database


  #return something


}



##Header for DNR remote sensing inventory project
#'@title
#'
#'  R Wrapper for FUSION gridmetrics
#'
#'@description
#'  R Wrapper for FUSION gridmetrics
#'  Connects to database with information on las tiles / project and initiates gridmetrics on a regular grids
#'
#'@details
#'
#'  This functions currently requires postgres database connectivity and that files were previously processed
#'  with las_status. DTM files must also be in the esoteric DEM format which is only supported by FUSION (Bob
#'  McGaughey) and minimally by lastools (Martin Isenburg).
#'
#'  Function works most efficiently if DEM files match las extents, possibly with buffers.
#'
#'  Tiles are processed on a regular gridswith some number of buffer pixels to guarantee that tiling issues
#'  can be avoided.
#'
#'  Function also optionally enables processing of subregions based on shapefiles.
#'
#'  Function operates by creating batch files that are fed to the sytem shell.
#'
#'  Current bug (bleh) requires that pixel width goes evenly into 66.
#'
#'  NOTE: gridmetrics fails to detect DEM boundaries - ie some of the data in an las tile may extend beyond
#'  the edge of a dem, and thus will have unreasonable height values. In areas of low elevation, this may cause
#'  strange / difficult to detect behavior.
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2015 July 17 Migrated to postgres from oracle \cr
#'1.0 \tab 2014 Dec 31 Roxygen Header formalized \cr
#'0.0 - 0.9 \tab 2012 - 2014 Various iterations \cr
#'}
#'
#'@author
#'Jacob Strunk <Jacob.strunk@@dnr.wa.gov>
#'
#'@param con database connection
#'@param las_table table with las data
#'@param las_path name of field in oracle which has path to las files
#'@param project_table table with las projects
#'@param metrics_table table of gridsmetrics
#'@param gridsgridsto process on, should have tile paths inside
#'@param dir_dtm directory of dtm files
#'@param out_dir where should gridmetrics dump
#'@param temp where should temporary files be placed
#'@param fusion_path path to fusion executables
#'@param mask_poly polygon to mask analysis
#'@param subset_sql polygon to subset las files
#'@param heightbreak fusion argument, height above which to compute lidar metrics
#'@param cellsize raster resolution
#'@param switches switches to gridmetrics
#'@param n_cores number of cores to process on at one time
#'@param logs create logs for analysis
#'@param execute should analysis start, or simply create batch files for fusion
#'@param del_temp delete temporary files
#'@param las_temp - non functional - eventually copy las files & dem to temp folder
#'@param require_dtms - should analysis require complete overlap with dtms
#'@param skip_existing_csv - should tiles with existing gridsmetrics be skipped?
#'@param refresh
#'
#'@return
#' NULL
#'
#'@examples
#'
#'
#'pg_gridmetrics(
#' las_table="LAS_STATUS"
#' ,project_table="project_STATUS"
#' ,mask_poly=if("test_dtm" %in% ls()) test_dtm else readOGR(paste(path_gis,"INVENTORY_TILES",sep="/"),"INVENTORY_TILES")
#' ,execute=F
#' ,switches="/cellbuffer:2 /first /minht:6 /outlier:-2,350"
#' ,n_cores=8
#' ,subset_sql="project_id in (33000127,33000080,33000003,33002930)"
#' ,refresh=F
#' ,require_dtms=F
#' ,out_dir="J:\\forest_info_1\\fris3\\data\\lidar_derivatives\\2014_Nov_21\\"
#' ,temp="J:\\forest_info_1\\fris3\\batch_processing\\fusion_temp5"
#' )
#'
#'@import raster rgdal
#'
#'@export
#
#'@seealso \code{\link{manage_las}}\cr \code{\link{manage_projects}}\cr \code{\link{read_dtm}}\cr  \code{\link{read_dtm_header}}\cr
#'

#Desired upgrades to this function:
##    1) include step to copy las and dtm tiles to temp drive
##    2) create permanent buffers for las tiles

##main function body

##name of function - PLACE NEW NAME HERE
pg_gridmetrics=function(
  con
  #tables and paths
  ,las_table="meta.las_status"
  ,las_path="full_path"
  ,project_table="meta.project_id"
  #,metrics_table=""
  ,arg_table="gridmetrics_manage.gridmetrics_args"
  ,arg_seq="gridmetrics_manage.gridmetrics_args_seq"
  ,dtm_status="meta.dtm_status"
  ,existing_arg_id=NA

  #,grids=if("path_gis" %in% ls(envir=parent.env(environment()))) readOGR(paste(get("path_gis",envir=parent.env(environment())),"INVENTORY_TILES",sep="/"),"INVENTORY_TILES")  else NA
  #,grids_path=paste(get("path_gis"),"INVENTORY_TILES/INVENTORY_TILES")
  ,dir_dtm="//Dnrfsoly107/fr_data/forest_info_1/fris3/data/fusion_dtms"#if("path_dtm" %in% ls(envir=parent.env(environment()))) get("path_dtm",envir=parent.env(environment()))
  ,out_dir=if("path_derivatives" %in% ls(envir=parent.env(environment()))) get("path_derivatives",envir=parent.env(environment())) else NA
  ,temp="c:\\temp\\process_gridmetrics\\"
  ,fusion_path=if("path_fusion" %in% ls(envir=parent.env(environment())))  paste(get("path_fusion",envir=parent.env(environment())),"gridmetrics.exe",sep="") else NA
  #,ogr2ogr_path="C:/OSGeo4W64/bin/ogr2ogr.exe"
  ,subset_sql="(las_status.project_YEAR > 2010 and  las_status.MINX >0 and  las_status.MINY>0 and las_status.MAXX <2556851 and las_status.MAXY <1362336 las_status.area_sqft  > 0 and las_status.area_sqmi < 5)"

  #fusion components
  ,heightbreak=6
  ,cellsize=66
  ,switches="/first /cellbuffer:2 /minht:6"

  #processing grid
  ,tile_size=1650
  ,dtm_tile_size=16500
  #,dtm_offset=16500/2+33
  ,extent=list(min_x = 561066+16500/2 - 33,min_y = 33066+16500/2 - 33, max_x = 2805066, max_y = 1551066,n_row=NA,n_col=NA)

  #processing configuration
  ,n_cores=5
  ,logs=TRUE
  ,execute=T
  ,del_temp=F
  ,las_temp=F                   #nonfunctional
  ,skip_existing_csv=T

  ,refresh=F
  ,verbose=T

  ,mask=NA
  ,mask_type=c("keep_intersection","remove_intersection")
  ,clus

){

  warning("I cannot create a new sequence and/or auto-set new values if sequence name changes")

  require(rgeos)
  require(sqldf)
  require(raster)
  require(rgdal)
  require(stringi)
  require(plyr)

  time_proc=format(Sys.time(), "%Y%b%d_H%HM%MS%S")

  temp=paste(temp,time_proc,sep="\\")

  #
  if(!is.na(existing_arg_id)){

    args_existing=pgReadTable(con,arg_table)
    match_id=match(existing_arg_id,args_existing[,"arg_id"])
    heightbreak=args_existing[match_id,"heightbreak"]
    cellsize=args_existing[match_id,"cellsize"]
    switches=args_existing[match_id,"switches"]
    arg_id=existing_arg_id

  }

  #prepare
  temp=temp[1]
  if(is.na(temp) | temp=="" | temp==FALSE) temp=dir_dtm
  if(!file_test("-d",temp)) dir.create(temp)

  #get list of tables in con
  tables_in=dbListTables(con)

  #fix paths
  out_dir=gsub("/","\\\\",gsub("\\\\$","",gsub("/$","",out_dir)))
  temp=gsub("/","\\\\",gsub("\\\\$","",gsub("/$","",temp)))
  dir_dtm=gsub("/","\\\\",gsub("\\\\$","",gsub("/$","",dir_dtm)))
  fusion_path=gsub("/","\\\\",gsub("\\\\$","",gsub("/$","",fusion_path)))

  if(!file_test("-d",out_dir)) dir.create(out_dir,recursive=T)
  if(!file_test("-d",temp)) dir.create(temp,recursive=T)

  #read in las tiles
  if(is.na(subset_sql))
    las_tiles = pgReadTable(con,las_table)
  else
    las_tiles = pgReadTable(con,paste("select * from ",las_table,"where",subset_sql,collapse=" "))



  if(nrow(las_tiles)==0) stop("no records found in las table - perhaps a problem with sql query")

  las_data=slot(las_tiles,"data")

  dtm_status=pgReadTable(con,dtm_status)

  #read in projects
  proj_data = pgReadTable(con,project_table,as_spatial=F, drop_geom = T, drop_rwkt = T)

  #subset las tiles by existing projects
  in_proj=as.numeric(las_data[,"project_id"]) %in% proj_data[,"project_id"]

  if(sum(in_proj)==0) {
    if(verbose)print(paste("No LAS files in",las_table,"match",project_table))
    return()
  }

  las_tiles=subset(las_tiles,subset=in_proj)
  las_data=subset(las_data,subset=in_proj)

  #get rid of bad tiles
  bad_xy=sapply(1:nrow(las_tiles),function(id,x)sum(unlist(bbox(x[id,]))<0)>0,las_tiles)

  if(sum(bad_xy)>0){
    warning("bad tile found, x or y coordinates less than zero when examining bounding box")
    print("bad_tile:")
    print(las_data[bad_xy,])

    las_tiles=subset(las_tiles,subset=!bad_xy)
    las_data=subset(las_data,subset=!bad_xy)
  }

  #apply a mask to data
  if(!is.na(mask)){

    require(maptools)
    mask_shp=
      readShapePoly(mask)
    spl_mask=split(mask_shp,1:nrow(mask_shp))
    res=do.call(cbind,lapply(spl_mask,gIntersects,las_tiles,byid=T))
    keep_mask=apply(res,1,function(x)sum(x)>0)
    if(mask_type[1]=="remove_intersection") keep_mask = !keep_mask
    las_tiles=subset(las_tiles,subset=keep_mask)
    las_data=subset(las_data,subset=keep_mask)

  }


  if(verbose)print("completed subsetting tiles that fall into projects")

  #make grids by project
  spl_las=split(las_tiles,las_tiles@data[,"project"])

  if(!inherits(clus,"cluster"))
    tile_overlaps=lapply(spl_las,function(...)try(.fn_make_grids(...)),tile_size,cellsize,dtm_tile_size,dir_dtm,extent=extent,dtm_status=dtm_status)
  if(inherits(clus,"cluster"))
    tile_overlaps=parLapply(clus,spl_las,function(...)try(.fn_make_grids(...)),tile_size,cellsize,dtm_tile_size,dir_dtm,extent=extent,dtm_status=dtm_status)

  #tile_overlaps=lapply(spl_las[has_err_over],function(...)try(.fn_make_grids(...)),tile_size,cellsize,dtm_tile_size,dir_dtm,extent=extent)
  has_err_over=sapply(tile_overlaps,class)=="try-error"
  if(sum(has_err_over)>0) warning("some projects were rejected when creating a grid overlay, it is likely their extents had problems:",names(spl_las[has_err_over]))
  tile_overlaps2=rbind.fill(tile_overlaps[!has_err_over])


  if(verbose)print("completed spatial_select_columns between las_buffers and las_tiles")

  #create text files with dtms and las files
  #create names for temporary text files
  dtm_txt_nms=paste("",temp,"\\DTM_LIST_",time_proc,"_",tile_overlaps2[,"project"],"_",tile_overlaps2[,"tile_id"],".txt",sep="")

  #add quotes to names
  nms_dtm_gud=lapply(tile_overlaps2[,"dtm_tiles"],backslash)
  names(nms_dtm_gud)=backslash(names(nms_dtm_gud))

  #actually create temporary files with dem names
  if(!inherits(clus,"cluster")) mapply(function(dtms_i,dtm_txt)writeLines(dtms_i,con=dtm_txt),nms_dtm_gud,dtm_txt_nms)
  #if(inherits(clus,"cluster")) clusterMap(clus,function(dtms_i,dtm_txt)writeLines(dtms_i,con=dtm_txt),nms_dtm_gud,dtm_txt_nms,.scheduling ="dynamic")
  if(inherits(clus,"cluster")) clusterMap(clus,.write_dtm_txt ,nms_dtm_gud,dtm_txt_nms,.scheduling ="dynamic")


  #create text files with las
  #create names for temporary las files
  las_txt_nms=paste("",temp,"\\LAS_LIST_",time_proc,"_",tile_overlaps2[,"project"],"_",tile_overlaps2[,"tile_id"],".txt",sep="")
  #add quotes to names
  las_files_gud=mapply(gsub,"XxXxYyYy","\\\\\\",mapply(gsub,"\\\\\\\\","\\\\",mapply(gsub,"^\\\\","XxXxYyYy",mapply(gsub,"/","\\\\",unlist(tile_overlaps2[,"las_tiles"],FALSE)))))
  names(las_files_gud)=tile_overlaps2[,"tile_id"]

  #actually create temporary files with las names
  if(!inherits(clus,"cluster")) mapply(function(las_i,las_txt)writeLines(las_i,con=las_txt),las_files_gud,las_txt_nms)
  #if(inherits(clus,"cluster")) clusterMap(clus,function(las_i,las_txt)writeLines(las_i,con=las_txt),las_files_gud,las_txt_nms,.scheduling ="dynamic")
  if(inherits(clus,"cluster")) clusterMap(clus,.write_las_txt,las_files_gud,las_txt_nms,.scheduling ="dynamic")

  if(verbose)print("completed temp file creation")

  #create buffers for tiles
  bbox_grid=paste("/gridxy:",paste(tile_overlaps2[,c("xmin")],tile_overlaps2[,c("ymin")],tile_overlaps2[,c("xmax")],tile_overlaps2[,c("ymax")],sep=","),sep="")

  #generate commands to process las tiles

  #prepare id for argument tracking
  if(is.na(existing_arg_id)) arg_id=dbGetQuery(con,paste("select nextval('",arg_seq,"')",sep=""))

  #create output csv
  #out_nms=paste(out_dir,"\\LIDAR_METRICS_",basename(slot(las_buffers,"data")[,"full_path"]),".csv",sep="")
  id_name=apply(cbind(tile_overlaps2[,"project"],tile_overlaps2[,"tile_id"]),1,paste,collapse="X_X")
  out_nms=paste(out_dir,"\\",tile_overlaps2[,"project"],"_argid_",arg_id,"\\LIDAR_METRICS_argid",arg_id,"_",id_name,".csv",sep="")

  #create project directories
  out_dirs=unique(dirname(out_nms))
  sapply(forwardslash(out_dirs),function(...)try(dir.create(...),silent=T),recursive=T)

  #ids_args=paste("las_id_",slot(las_buffers,"data")[,"las_id"],"_arg_id_",arg_id,sep="")
  ids_args=paste(id_name,",",arg_id,sep="")
  warning("arg_id is being appended to the fusion csv output through the identifier"
          ," switch using a comma: \n\n e.g /identifier:239425,35 \n\n"
          ," this will break some csv readers!! They will assume that the first column has row names, which is not correct."
          ," the result is that names are offset from rows by one field..."
          ,"Dealing with this requires a hand tuned csv reader..."
  )

  #create commands
  if(!grepl("/noground",switches))
    commands_in=mapply(paste,paste("\"",fusion_path,"\"",sep=""), bbox_grid,switches,paste("/id:",ids_args,sep=""),paste("\"",dtm_txt_nms,"\"",sep=""),heightbreak,cellsize,paste("\"",out_nms,"\"",sep=""),paste("\"",las_txt_nms,"\"",sep=""))
  else
    commands_in=mapply(paste,paste("\"",fusion_path,"\"",sep=""), bbox_grid,switches,paste("/id:",ids_args,sep=""),heightbreak,cellsize,paste("\"",out_nms,"\"",sep=""),paste("\"",las_txt_nms,"\"",sep=""))

  #update argument tracking table
  arg_info=data.frame(arg_id=unlist(arg_id)
                      ,creation_date=as.character(Sys.time())
                      ,heightbreak=heightbreak
                      ,cellsize=cellsize
                      ,switches=switches
                      ,output_directory=out_dir
                      ,temp_folder=temp
                      ,fusion_path=fusion_path
                      ,example_call=commands_in[1]
                      ,row.names=NULL
                      ,tile_size=tile_size
  )
  if(is.na(existing_arg_id)) pgWriteTable(con,arg_table,arg_info)

  #skip existing csv files
  if(skip_existing_csv){

    #test names
    out_files_old=list.files(out_dir,pattern="elevation_stats.csv",recursive=T)
    short_old1=gsub("_first_returns_elevation_stats.csv","",gsub("LIDAR_METRICS_","",basename(out_files_old)))
    short_old2=gsub("_all_returns_elevation_stats.csv","",gsub("LIDAR_METRICS_","",basename(short_old1)))
    short_new=paste(paste("argid",arg_id,sep=""),id_name,sep="_")
    exist= tolower((short_new)) %in%  tolower((short_old2  ))

    cat(sum(exist),"las files were omitted from the supplied list of ",length(exist),"files because they already exist in the outputs")

    commands_in=commands_in[!exist]

  }

  #make file perform multiple operations

  #create batch file names
  create_time=format(Sys.time(), "%Y_%a_%b_%d_%Hp%Mp%S")
  out_bats=paste(temp,"\\",create_time,"_GRIDMETRICS_BATCH_FILE_",1:n_cores,".bat",sep="")
  out_master=paste(temp,"\\",create_time,"_GRIDMETRICS_BATCH_FILE_master.bat",sep="")

  #cut commands by number of cores
  id_bats=cut(sample(1:length(commands_in)),breaks=n_cores,labels=FALSE)

  #send results to log files
  if(logs){

    out_logs=paste(">> ",out_bats[id_bats],"_OUTPUT.txt & type ",out_bats[id_bats],"_OUTPUT.txt",sep="")
    commands_in=mapply(paste,commands_in,out_logs,collapse=" ")

  }

  #split commands by id
  spl_commands=split((commands_in),id_bats)

  #add commands to delete intermediate steps
  if(del_temp) spl_commands=
    mapply(function(coms,fils1,fils2)c(unlist(coms),unlist(paste("rm",c(fils1,fils2)))),spl_commands,split(dtm_txt_nms,id_bats),split(las_txt_nms,id_bats),SIMPLIFY=F)

  #send commands to batch files
  mapply(writeLines, spl_commands,out_bats)

  #create master batch file to call other batch files without having to execute them all by hand
  writeLines(paste("start",out_bats),out_master)

  #execute command directly
  if(execute){

    sapply(out_bats,function(x,...) system(x,...),invisible=F,wait=F)

  }
  NULL

}
#used to write paths to las and dtm files to text files in parallel
.write_las_txt=function(las_i,las_txt)writeLines(las_i,con=las_txt)
.write_dtm_txt=function(dtms_i,dtm_txt)writeLines(dtms_i,con=dtm_txt)

.make_polys=function(xyi,cellsize,clus=NA){

  .make_polys_in = function(xyi_i,cellsize){
    #polyi = Polygon(cbind(c(xyi_i[,1],xyi_i[,1] + cellsize,xyi_i[,1]+cellsize,xyi_i[,1],xyi_i[,1]),c(xyi_i[,2]+cellsize,xyi_i[,2]+cellsize,xyi_i[,2],xyi_i[,2],xyi_i[,2]+cellsize)))
    polyi = Polygon(cbind(c(xyi_i[,1] - cellsize/2,xyi_i[,1] + cellsize/2,xyi_i[,1]+cellsize/2,xyi_i[,1]- cellsize/2,xyi_i[,1]- cellsize/2),c(xyi_i[,2]+cellsize/2,xyi_i[,2]+cellsize/2,xyi_i[,2]- cellsize/2,xyi_i[,2]- cellsize/2,xyi_i[,2]+cellsize/2)))
    return(Polygons(list(polyi),ID=xyi_i[,"id"]))
  }

  xyi=xyi[!duplicated(xyi[,"id"]),]
  if(!inherits(clus,"cluster"))poly1 = lapply(split(xyi,xyi[,"id"]),.make_polys_in,cellsize)
  if(inherits(clus,"cluster"))poly1 = parLapply(clus,split(xyi,xyi[,"id"]),.make_polys_in,cellsize)
  poly2 = SpatialPolygons(poly1)
  SpatialPolygonsDataFrame(poly2,data.frame(id=xyi[,"id"],row.names=xyi[,"id"],x_min = xyi[,"id"],y_min=xyi[,"id"],row_col=xyi[,"row_col"]))
}

.fn_make_grids=function(x,tile_size,cellsize,dtm_tile_size,dir_dtm,dtm_offset,extent,dtm_status){

  #create initial grid of processing tiles
  bb_i=bbox(x)
  grid_i=raster_grid_xy(bb_i[1,1],bb_i[1,2],bb_i[2,1],bb_i[2,2],tile_size,extent=extent)

  # #crop grid, eliminating points that don't interesect tiles with buffers t
  #     #cast grid to spatial type
  #     grid_i_spdf=grid_i
  #     coordinates(grid_i_spdf)=~x+y
  #
  #     #buffer tiles the width of the processing tiles
  #     x_buf=gSimplify(buffer(x,tile_size+3*cellsize),5)
  #
  #     #intersect buffered tiles with grid cells
  #     local({
  #         setTimeLimit(5)
  #         int1=gIntersects(x_buf,grid_i_spdf,byid=T)
  #     })
  #
  #     #subset grid where there is no intersection
  #     grid_i_spdf2=subset(grid_i_spdf,int1)
  #     grid_i_spdf2[,c("x","y")]=coordinates(grid_i_spdf2)
  #     grid_i_subset=as.data.frame(grid_i_spdf)

  #create tiles from subset grid
  #browser()
  #poly_grid=.make_polys(grid_i,tile_size+3*cellsize,clus=makeCluster2(30))
  poly_grid=.make_polys(grid_i,tile_size+3*cellsize)


  #grid_i_dtm=raster_grid_xy(bb_i[1,1],bb_i[1,2],bb_i[2,1],bb_i[2,2],dtm_tile_size,extent=extent)
  #poly_grid_dtm=.make_polys(grid_i_dtm,dtm_tile_size)
  #writeSpatialShape(poly_grid_dtm,"c:/temp/test4.shp")

  tile_on_las=spatial_select_column(
    input_shape = poly_grid
    ,select_shape = x
    ,input_id= "id"
    ,select_id = "full_path"
    ,prefix=""
    ,suffix=""
    ,must_contain=F
  )

  # tile_on_dtm=spatial_select_column(
  #                          input_shape = poly_grid
  #                          ,select_shape = dtm_status
  #                          ,input_id= "id"
  #                          ,select_id = "dtm_id"
  #                          ,prefix=paste(dir_dtm,"\\",sep="")
  #                          ,suffix=".dtm"
  #                          ,must_contain=F
  #                          )

  tile_on_dtm=spatial_select_column(
    input_shape = poly_grid
    ,select_shape = dtm_status
    ,input_id= "id"
    ,select_id = "dtm_id"
    ,prefix=""#paste(dir_dtm,"\\",sep="")
    ,suffix=""#".dtm"
    ,must_contain=F
  )
  tile_on_dtm2=sapply(tile_on_dtm,function(x,y,z)if(!is.null(x))paste(y,x,z,sep=""),paste(dir_dtm,"\\",sep=""),".dtm")
  bb_grid_i=lapply(split(poly_grid,1:nrow(poly_grid)),bbox)

  res_df=data.frame(
    project=x@data$project[1]
    ,project_id=x@data$project_id[1]
    ,tile_id=grid_i[,3]
    ,rbind.fill(lapply(bb_grid_i,function(x)data.frame(xmin=x[1,1],xmax=x[1,2],ymin=x[2,1],ymax=x[2,2])))
    ,las_tiles=sapply(tile_on_las,paste,collapse="\n")
    ,dtm_tiles=sapply(tile_on_dtm2,paste,collapse="\n")
    ,row.names=NULL
  )

  has_overlap=sapply(tile_on_las,function(x) length(x)>0)
  res_df[has_overlap,]
  #list(bb_grid_i[has_overlap],tile_on_las[has_overlap],tile_on_dtm[has_overlap])
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

