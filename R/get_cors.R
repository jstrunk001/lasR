
#cors KMZ: https://www.ngs.noaa.gov/CORS_Map/corsdownload?id=kmz

<<<<<<< HEAD
get_base=function(from,to,xy,n=1,ftp="",download = F,cors_kmz ="" ,get=c(".gz",".Z")){
=======
get_base=function(from,to,xy,n=1,ftp="",download = F,cors_kmz = ,get=c(".gz",".Z")){
>>>>>>> a843423edeee20444e4156161b17805bf5ae3203

  library(maptools)
  cds = getKMLcoordinates(textConnection(system("unzip -p /Users/foo/test.kmz", intern = TRUE)))


}
.download_base=function(){



}

.choose_nearest=function(xy,ftp){





}


