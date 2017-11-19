#'@title
#'  scan las files and add them to summary table
#'
#'@description
#'  scan las files and add them to summary table
#'
#'@details
#'  scan las files and add them to summary table
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 March 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@fs.fed.us>
#'
#'@param project name of lidar project
#'@param project_year year of lidar project
#'@param proj4_name human interpretable name of projection
#'@param proj4 proj4 string for projection
#'@param dir_las where to find lidar files
#'@param pattern pattern to use in searching for las files
#'@param notes and descriptionn that may be helpful in using a project
#'@param create_polys output shapefiles of polygon bboxes
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'scan_las(project="test1", project_year="2015",dir_las="C:\\temp\\lidar_test\\",con=con_inv)
#'
#'@import maptools sp uuid rgdal
#'
#'@export
#
#'@seealso \code{\link{scan_dtm}}\cr \code{\link{read_las}}\cr


scan_las=function(
    project="some_project"
    ,project_year="2099"
    ,proj4_name="NAD_1983_HARN_StatePlane_Washington_South_FIPS_4601_Feet"
    ,proj4="1395 +proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
    ,dir_las=""
    ,pattern="[.]la.$"
    ,notes=""
    ,create_polys=T
    ){
  require(uuid)
  require(rgdal)

  proc_date=Sys.time()

  files_las=list.files(dir_las,full.names=T,recursive=F,include.dirs = FALSE,pattern=pattern)
  if(length(files_las)==0) stop("'scan_las' argument dir_las is not a directory or is empty")

  #prepare / read project_id file
  project_id_folder=paste(dir_las,"/manage_las/",sep="")
  project_id_csv=paste(project_id_folder,"project_id.csv",sep="")
  las_id_csv=paste(project_id_folder,"las_id.csv",sep="")

  exist_project_id_folder=dir.exists(project_id_folder)
  exist_project_id_csv=file.exists(project_id_csv)
  exist_las_id_csv=file.exists(las_id_csv)

  if(!exist_project_id_folder) dir.create(project_id_folder,recursive=T)
  if(exist_project_id_csv & exist_las_id_csv){

    project_id_df=read.csv(project_id_csv)
    las_id_df = read.csv(las_id_csv)

  }else{

    las_id_df = data.frame()

    project_id_df=data.frame(
      project_id=UUIDgenerate(use.time = NA)
      ,project=project
      ,project_year=project_year
      ,load_date=proc_date
      ,file_path=dir_las
      ,notes=notes
      ,proj4_name=proj4_name
      ,proj4=proj4
    )
    write.csv(project_id_df, project_id_csv,row.names=F)

  }

  proj_id=project_id_df[1,"project_id"]

  #write little disclaimer / meta file to folder e.g. what is this crap in this folder
  disclaimer="This folder contains files used to inventory las/laz files."
  disclaimer_txt=paste(project_id_folder,"DISCLAIMER.txt",sep="")
  writeLines(disclaimer,disclaimer_txt)

  #check if las files exist
  names_las=basename(files_las)
  names_las_exist = names_las %in% las_id_df$file_name
  las_update = sum(!names_las_exist) > 0

  #update las
  if(las_update){
    print("las_update")
    #identify missing records
    files_las=files_las[!names_las_exist]

    #get lidar headers
    headers=read_header(files_las)

    #prep data for database
    names(headers)=gsub("max","max_",gsub("min","min_",tolower(names(headers))))
    headers[,"project_id"]=proj_id
    headers[,"project"]=project
    headers[,"project_year"]=project_year
    headers[,"las_id"]=sapply(1:nrow(headers),function(x)UUIDgenerate())
    headers[,"file_name"]=basename(files_las)
    headers[,"file_path"]=files_las
    headers[,"load_date"]=proc_date
    headers[,"notes"]=notes
    las_id_df=rbind(headers,las_id_df)
    write.csv(las_id_df,las_id_csv)

  }


  if(create_polys){

    las_id_df=read.csv(las_id_csv,stringsAsFactors =F)

    polys_rds=paste(project_id_folder,"las_polys.rds",sep="")
    polys_shp=paste(project_id_folder,"las_polys.shp",sep="")

    las_polys=bbox2polys(las_id_df[,c("las_id","min_x","max_x","min_y","max_y")])
    row.names(las_id_df)=las_id_df[,"las_id"]

    las_polys=sp::SpatialPolygonsDataFrame(las_polys,las_id_df)

    #save outputs
    try(saveRDS(las_polys,polys_rds))
    try(rgdal::writeOGR(las_polys,dsn=dirname(polys_shp),layer=basename(polys_shp),driver="ESRI Shapefile"))

  }


}


