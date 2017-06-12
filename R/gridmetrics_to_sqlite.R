#install.packages("RSQLite");install.packages('sqldf')

library(sqldf)

#folders
dir_csv="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\gridmetrics_csv\\"
dir_sqlite="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\gridmetrics_sqlite\\"
dir.create(dir_sqlite)

#get list of csv files
csv_files=list.files(dir_csv,full.names=T,pattern="[.]csv")

#create sqlite database
db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics.db"),sychronous = "off")
tab_summ="summ_naip_2015_66foot"
tab_gm="gm_naip_2015_66foot"

#create summary table
tabs=dbListTables(db)
summ_tab=data.frame(tile=gsub("_.*","",basename(csv_files)),llx=NA,lly=NA,urx=NA,ury=NA,file=csv_files,project="naip_2015",res="66 feet",projection="WA State Plane South NAD83(HARN)",status="not_loaded",notes="")
if(!tab_summ %in% tabs) dbWriteTable(db, tab_summ,summ_tab)

#load files into sqlite database
summ_tab_2=dbGetQuery(db, paste("select * from",tab_summ,"where status = 'not_loaded'" ))
#for(i in 1:nrow(summ_tab_2)){
for(i in 1:3){
    file_i=summ_tab_2[i,"file"]
    tile_i=summ_tab_2[i,"tile"]
    dati=read.csv(file_i)
    names(dati)=tolower(names(dati))
    #eventually fix names here
    try(dbWriteTable(conn=db,name=tab_gm,value=dati,append=T))
    dbGetQuery(db, paste("update",tab_summ,"set status = 'loaded' where tile =",tile_i))
    print(file_i)

}
