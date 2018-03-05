   if(F){

  library(lasR)
  library(rgdal)
  library(rgeos)
  library(raster)
  library(maptools)
  library(helpers)

  #get projection info
  source("C:\\box\\BoxSync\\sync\\R\\my_packages\\helpers\\R\\get_proj4.r")
  get_proj4(details=c("HARN","wash","ftUS","nad83"))
  prj_to="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  get_proj4(details=c("HARN","nad83"))
  prj_from="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

  plots0=readOGR("D:\\projects\\2017_WA_DSM_Pilot\\GIS\\merged_Wasp.shp",stringsAsFactors=F)
  plots1=subset(plots0,subset=plots0@data$statecd==53)
  plots2=gBuffer(plots1,width=24,byid=T)
  plots3=aggregate(plots2,by=c("plot"))

  proj=readOGR("D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project003.shp",stringsAsFactors=F)


  clip_plots(plot_polys=plots3
             ,lasR_project_polys=proj
             ,dir_out="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\plot_clips"
             ,n_core=20
  )

  clip_plots(plot_polys=plots3
             ,lasR_project_polys=proj
             ,dir_out="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\plot_clips_elev"
             ,n_core=20
             ,height=F
  )

}
