if(F){
  library(lasR)
  library(rgdal)
  library(rgeos)
  library(raster)
  library(maptools)

  plots0=readOGR("D:\\projects\\2017_WA_DSM_Pilot\\GIS\\merged_Wasp.shp",stringsAsFactors=F)
  plots1=subset(plots0,subset=plots0@data$statecd==53)
  plots2=gBuffer(plots1,width=24)
  plots3=aggregate(plots2,by=c("plot"))

  proj=readOGR("D:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP\\lasR_project001.shp")

  clip_plots(plot_polys=plots3
             ,lasR_project_polys=proj
             ,dir_out="D:\\projects\\2017_WA_DSM_Pilot\\plot_clips\\2017_12_01\\"
             ,n_core=20
  )
}
