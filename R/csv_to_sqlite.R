



csv_to_sqlite=function(db
                       ,csv_folder
                       ,tb_summary="gm_summary"
                       ,tb_csv="gm"
                       ,project="naip_2015"
                       ,resolution="66"
                       ,units="feet"
                       ,proj4=""
                       ,notes=""

                       ){

  require(RSQLite)

  csv_files=list.files(csv_folder,full.names=T,pattern="[.]csv")

  dat0=read.csv(csv_files[1])
  names(dat0)=gsub("[.]+","_",gsub("^x[.]","",tolower(names(dat0))))
  names0=names(dat0)
  dbWriteTable(db,tb_csv,dat0[0,],overwrite = TRUE,append=F)

  errs=list()
  n_files=length(csv_files)

  for(i in 1:n_files){

    dati=read.csv(csv_files[i])
    names(dati)=names0
    dati=dati[ dati[,"total_all_returns"] > -1 , ]
    if(nrow(dati)>0) errs[i]=try(dbWriteTable(db,tb_csv,dati,append=T))
    print(paste(i,"from",n_files,"files at",Sys.time()))

  }


}

if(F){

  dir_csv="C:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\gridmetrics\\gridmetrics_csv\\"
  dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\sqlite\\"
  dir.create(dir_sqlite)

  require(RSQLite)
  db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics.db"),sychronous = "off")


  csv_to_sqlite(db=db
                ,csv_folder=dir_csv
                )

}
