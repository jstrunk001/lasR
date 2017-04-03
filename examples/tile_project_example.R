
tile_project(
  dir_las="C:\\Temp\\las_test\\"
  ,dir_dtm="C:\\Temp\\dtm_test\\"
  ,dir_project="C:\\Temp\\naip_2015_t1650_p66\\"
  ,project="test_project"
  ,project_dtm="some_project"
  ,project_las="some_project"
  ,dtm_year="2099"
  ,dlas_year="2099"
  ,scan_dtms=T
  ,scan_las=T
  ,create_project=T
  ,tile_size=1650
  ,pixel_size=66
  ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
  ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
)

library(rgdal)
proj_area=readOGR(dsn="C:\\projects\\2017_WA_DSM_Pilot",layer="extent")

tile_project(
  dir_las="G:\\phodar\\NAIP_2015\\las_files\\"
  ,dir_dtm="F:\\DNR\\FUSION_DTMS\\"
  ,dir_project="C:\\projects\\2017_WA_DSM_Pilot\\"
  ,project="DSM_Pilot_A"
  ,project_dtm="naip"
  ,project_las="naip"
  ,dtm_year="2015"
  ,las_year="2015"
  ,mask=proj_area
  ,scan_dtms=T
  ,scan_las=T
  ,create_project=T
  ,tile_size=1650
  ,pixel_size=66
  ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
  ,crs="+proj=lcc +lat_1=47.33333333333334 +lat_2=45.83333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000.0001016001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
)
