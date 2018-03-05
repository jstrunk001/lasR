
library(lasR)
dir_csv="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_07\\gridmetrics_csv\\"
#dir_csv="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\test\\"
dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\sqlite\\"
dir.create(dir_sqlite)

require(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics_usgs007e.db"),sychronous = "off")

csv_to_sqlite(db=db
              ,csv_folder=dir_csv
              #,n_load=150
              ,skip_loaded=T
)





###Debug

if(F){

  dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\sqlite"
  require(RSQLite)
  db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics_usgs007test.db"),sychronous = "off")
  summ=dbReadTable(db,"gm_summary")
  gm=dbReadTable(db,"gm")

  summ[grep(93421,summ$file),]


}
