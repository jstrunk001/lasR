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
#'@param project name of lidar project
#'@param project_year year of lidar project
#'@param proj4_name human interpretable name of projection
#'@param proj4 proj4 string for projection
#'@param dir_dtm where to find dtm files
#'@param con  postgres connection
#'@param dtm_table table to put individual dtm tile records
#'@param project_table table with dtm projects (e.g. for a state or project...)
#'@param pattern pattern to use in searching for las files
#'@param notes and descriptionn that may be helpful in using a project
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import RPostgreSQL
#'
#'@export
#
#'@seealso \code{\link{scan_las}}\cr \code{\link{read_dtm}}\cr \code{\link{read_dtm_header}}\cr

#Desired upgrades to this function:
#
#



scan_dtm=function(
  project="wa_dtm"
  ,project_year="2099"
  ,proj4_name="NAD_1983_HARN_StatePlane_Washington_South_FIPS_4601_Feet"
  ,proj4="1395 +proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  ,dir_dtm=""
  ,con
  ,dtm_table="manage_dtm.dtm_files"
  ,project_table="manage_dtm.dtm_projects"
  ,pattern="[.]dtm$"
  ,notes=""

  ){

  dtm_table2=unlist(strsplit(dtm_table,"[.]"))
  project_table2=unlist(strsplit(project_table,"[.]"))

  proc_date=Sys.time()

  files_dtm=list.files(dir_dtm,full.names=T,recursive=F,include.dirs = FALSE,pattern=pattern)
  if(length(files_dtm)==0) stop("'scan_dtm' argument dir_dtm is not a directory or is empty")

  #get project id if it exists
  proj_id=unlist(dbGetQuery(con,paste("select project_id from",project_table,"where project = ",paste("'",project,"'",sep=""),"and project_year = ",project_year)))

  #prepare / read project_id file
  project_id_folder=paste(dir_dtm,"/manage_dtm/",sep="")
  project_id_csv=paste(project_id_folder,"project_id.csv",sep="")
  exist_project_id_folder=dir.exists(project_id_folder)
  exist_project_id_csv=file.exists(project_id_csv)

  if(!exist_project_id_folder) dir.create(project_id_folder)
  if(exist_project_id_csv) project_id_df=read.csv(project_id_csv)

  #write little disclaimer / meta file to folder e.g. what is this crap in this folder
  disclaimer="This folder contains files used to inventory dtm files."
  disclaimer_txt=paste(project_id_folder,"DISCLAIMER.txt",sep="")
  writeLines(disclaimer,disclaimer_txt)

  #read in data
  db_dtm=dbGetQuery(con,paste("select * from ",dtm_table))
  db_projects=dbGetQuery(con,paste("select * from ",project_table))

  #check if project / year exists
  project_exists = project %in% db_projects$project & project_year %in% db_projects$project_year

  #check if dtm files exist
  names_dtm=basename(files_dtm)
  names_dtm_exist = names_dtm %in% db_dtm$file_name & project_exists
  dtm_update = sum(!names_dtm_exist) > 0


  #add project ids
  if(!exist_project_id_csv | is.null(proj_id)){

    #create project id
    if(is.null(proj_id)){
      proj_id_max=max(20000,unlist(dbGetQuery(con,paste("select max(project_id) from",project_table))),na.rm=T)
      proj_id=proj_id_max+1
    }

    #project_id_df=data.frame(project_id=proj_id,project=project,project_year=project_year,load_date=proc_date,file_path=dir_dtm,notes="")

    project_id_df=data.frame(
      project_id=proj_id
      ,project=project
      ,project_year=project_year
      ,load_date=proc_date
      ,file_path=dir_dtm
      ,notes=notes
      ,proj4_name=proj4_name
      ,proj4=proj4
    )
    write.csv(project_id_df, project_id_csv,row.names=F)
    cols_proj_db=dbListFields(con,project_table2)
    dbWriteTable(con,project_table2,project_id_df[cols_proj_db],append=T,row.names=F)

  }

  if(dtm_update){

    #identify missing records
    files_dtm=files_dtm[!names_dtm_exist]

    #get lidar headers
    headers=read_dtm_header(files_dtm)

    #generate ids
    dtm_id_max=max(0,unlist(dbGetQuery(con,paste("select max(dtm_id) from",dtm_table))),na.rm=T)
    ids_dtm=seq(1:nrow(headers))+dtm_id_max

    #prep data for database
    names(headers)=gsub("max","max_",gsub("min","min_",tolower(names(headers))))
    headers[,"project_id"]=proj_id
    headers[,"project"]=project
    headers[,"project_year"]=project_year
    headers[,"dtm_id"]=ids_dtm
    headers[,"file_name"]=basename(files_dtm)
    headers[,"file_path"]=files_dtm
    headers[,"load_date"]=proc_date
    headers[,"notes"]=notes
    headers[,"min_x"]=headers[,"ll_x"]
    headers[,"min_y"]=headers[,"ll_y"]
    headers[,"max_x"]=headers[,"min_x"]+headers[,"n_cols"]*headers[,"col_spacing"]
    headers[,"max_y"]=headers[,"min_y"]+headers[,"n_rows"]*headers[,"row_spacing"]

    #write data to file
    cols_dtm_db=dbListFields(con,dtm_table2)
    cols_dtm_db_ok=cols_dtm_db %in% names(headers)

    if(sum(!cols_dtm_db_ok)>0)
      stop(paste("Column names in database table",dtm_table,"are not correct for 'scan_dtm'!\n They should be: project_id,project,project_year,dtm_id,file_name,load_date,min_x,min_y,max_x,max_y,file_path,notes"))
    headers_cols=headers[,cols_dtm_db]
    dbWriteTable(con,dtm_table2,headers_cols,append=T,row.names=F)

  }

  }
