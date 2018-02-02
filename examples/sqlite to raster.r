dir_sqlite="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\sqlite"

require(RSQLite)
db <- dbConnect(SQLite(), dbname=file.path(dir_sqlite,"2015_NAIP_Metrics_usgs007.db"),sychronous = "off")


x=dbGetQuery(db,"select center_x,center_y,ht_p95 from gm")
x1=x[!is.na(x$center_x) & !is.na(x$center_y)  ,  ]
x1[x1==-9999] = 0

library(raster)
r1=rasterFromXYZ(x1[,c("center_x","center_y","ht_p95")])
writeRaster(r1,"C:\\projects\\2017_WA_DSM_Pilot\\2017Dec_NAIP_usgs\\rasters\\ht_p95_007c.img", datatype='FLT4S',overwrite=TRUE)
gc()
