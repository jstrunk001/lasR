#'@title
#'  scan las files and add them to summary table
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
#'Jacob Strunk <Jstrunk@fs.fed.us>
#'
#'@param project name of lidar project
#'@param project_year year of lidar project
#'@param proj4_name human interpretable name of projection
#'@param proj4 proj4 string for projection
#'@param dir_las where to find lidar files
#'@param con  postgres connection
#'@param las_table table to put individual las tile records
#'@param project_table table with las projects (e.g. an acquisition)
#'@param pattern pattern to use in searching for las files
#'@param notes and descriptionn that may be helpful in using a project
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples

#'if(!"con_inv" %in% ls()){
#'  library("RPostgreSQL")
#'  con_inv=dbConnect(dbDriver("PostgreSQL"),dbname="inventory",host="localhost",port="5432",user="postgres",password="0000")
#'}
#'scan_las(project="test1", project_year="2015",dir_las="C:\\temp\\lidar_test\\",con=con_inv)
#'
#'@import RPostgreSQL, maptools
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr


scan_las=function(
    project="some_project"
    ,project_year="2099"
    ,proj4_name="NAD_1983_HARN_StatePlane_Washington_South_FIPS_4601_Feet"
    ,proj4="1395 +proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
    ,dir_las=""
    ,con
    ,las_table="manage_las.las_files"
    ,project_table="manage_las.las_projects"
    ,pattern="[.]la.$"
    ,create_polys=T
    ){

  las_table2=unlist(strsplit(las_table,"[.]"))
  project_table2=unlist(strsplit(project_table,"[.]"))

  proc_date=Sys.time()

  require(RPostgreSQL)

  files_las=list.files(dir_las,full.names=T,recursive=F,include.dirs = FALSE,pattern=pattern)
  if(length(files_las)==0) stop("'scan_las' argument dir_las is not a directory or is empty")

  #get project id if it exists
  proj_id=unlist(dbGetQuery(con,paste("select project_id from",project_table,"where project = ",paste("'",project,"'",sep=""),"and project_year = ",project_year)))

  #prepare / read project_id file
  project_id_folder=paste(dir_las,"/manage_las/",sep="")
  project_id_csv=paste(project_id_folder,"project_id.csv",sep="")
  exist_project_id_folder=dir.exists(project_id_folder)
  exist_project_id_csv=file.exists(project_id_csv)

  las_id_csv=paste(project_id_folder,"las_id.csv",sep="")

  if(!exist_project_id_folder) dir.create(project_id_folder)
  if(exist_project_id_csv) project_id_df=read.csv(project_id_csv)

  #write little disclaimer / meta file to folder e.g. what is this crap in this folder
  disclaimer="This folder contains files used to inventory las/laz files."
  disclaimer_txt=paste(project_id_folder,"DISCLAIMER.txt",sep="")
  writeLines(disclaimer,disclaimer_txt)

  #read in data
  db_las=dbGetQuery(con,paste("select * from ",las_table))
  db_projects=dbGetQuery(con,paste("select * from ",project_table))

  #check if project / year exists
  project_exists = project %in% db_projects$project & project_year %in% db_projects$project_year

  #check if las files exist
  names_las=basename(files_las)
  names_las_exist = names_las %in% db_las$file_name & project_exists
  las_update = sum(!names_las_exist) > 0

  #add project ids
  if(!exist_project_id_csv | is.null(proj_id)){

    #create project id
    if(is.null(proj_id)){
      proj_id_max=max(9000,unlist(dbGetQuery(con,paste("select max(project_id) from",project_table))),na.rm=T)
      proj_id=proj_id_max+1
    }

    project_id_df=data.frame(
        project_id=proj_id
        ,project=project
        ,project_year=project_year
        ,load_date=proc_date
        ,file_path=dir_las
        ,notes=""
        ,proj4_name=proj4_name
        ,proj4=proj4
        )

    write.csv(project_id_df, project_id_csv,row.names=F)
    cols_proj_db=dbListFields(con,project_table2)
    dbWriteTable(con,project_table2,project_id_df[cols_proj_db],append=T,row.names=F)

  }

  if(las_update){

    #identify missing records
    files_las=files_las[!names_las_exist]

    #get lidar headers
    headers=read_header(files_las)

    #generate ids
    las_id_max=max(0,unlist(dbGetQuery(con,paste("select max(las_id) from",las_table))),na.rm=T)
    ids_las=seq(1:nrow(headers))+las_id_max

    #prep data for database
    names(headers)=gsub("max","max_",gsub("min","min_",tolower(names(headers))))
    headers[,"project_id"]=proj_id
    headers[,"project"]=project
    headers[,"project_year"]=project_year
    headers[,"las_id"]=ids_las
    headers[,"file_name"]=basename(files_las)
    headers[,"file_path"]=files_las
    headers[,"load_date"]=proc_date
    headers[,"notes"]=proc_date
    headers[,"proj4_name"]=proj4_name
    headers[,"proj4"]=proj4

    #write data to file
    cols_las_db=dbListFields(con,las_table2)
    cols_las_db_ok=cols_las_db %in% names(headers)
    if(sum(!cols_las_db_ok)>0)
      stop(paste("Column names in database table",las_table,"are not correct for 'scan_las'!\n They should be: project_id,project,project_year,las_id,file_name,load_date,min_x,min_y,max_x,max_y,file_path,notes"))
    headers_cols=headers[,cols_las_db]
    dbWriteTable(con,las_table2,headers_cols,append=T,row.names=F)

    write.csv(headers_cols,las_id_csv, row.names = F, append=F)

  }

  if(create_polys){
    browser()
    headers_cols=dbReadTable(con,las_table2,headers_cols,append=T,row.names=F)

    polys_rds=paste(project_id_folder,"las_polys.rds",sep="")
    polys_shp=paste(project_id_folder,"las_polys.shp",sep="")

    las_polys=bbox2polys(headers_cols[headers_cols,c("las_id","min_x","max_x","min_y","max_y")])
    saveRDS(las_polys,polys_rds)
    writePolyShape(las_polys,polys_shp)

  }


}


