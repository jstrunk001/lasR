
run_gridmetrics=function(

  tile_project="c:/temp/test_project/intersections.csv"
  ,dir_out="c:/temp/test_project/gridmetrics"
  ,n_core=4
  ,gridmetrics=c("c:\\fusion\\gridmetrics.exe","lasR")
  ,heightbreak=6
  ,cellsize=66
  ,minht=6
  ,first=T
  ,intensity=F
  ,outlier=c(-5,400)
  ,fusion_switches="/nointensity /first"
  ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
  ,fns=list(min=min,max=max,mean=mean,sd=sd,p20=function(x,...)quantile(x,.2,...),p75=function(x,...)quantile(x,.2,...),p95=function(x,...)quantile(x,.2,...))

  ){
  gridmetrics=gridmetrics[1]
  do_fusion = F
  if(class(gridmetrics[1])=="character"){
    if(grepl("gridmetrics.exe",gridmetrics)) do_fusion = T
  }

  #time stamp for outputs
  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")

  #create temp folder
  gm_out=backslash(paste(dir_out,"/gridmetrics_csv/",proc_time,"/",sep=""))
  if(!dir.exists(gm_out)) dir.create(gm_out,recursive=T)

  #create csv folder dum
  temp=backslash(paste(dirname(tile_project),"/run_gridmetrics_temp/",proc_time,"/",sep=""))
  if(!dir.exists(temp)) dir.create(temp)

  #get project data
  tile_dat=read.csv(tile_project)

  #prepare batch commands
  if(do_fusion){

    tile_dat[,"dtm_txt"]=backslash(paste(temp,tile_dat[,"tile_id"],"_dtm.txt",sep=""))
    tile_dat[,"las_txt"]=backslash(paste(temp,tile_dat[,"tile_id"],"_las.txt",sep=""))
    tile_dat[,"switches"]=paste("/minht:",minht
                              ," /outlier:",paste(outlier,collapse=",")
                              ," /cellbuffer:2 /gridxy:"
                              ,apply(tile_dat[,c("mnx","mny","mxx","mxy")],1,paste,collapse=",")
                              ,sep="")
    tile_dat[,"outf"]=paste(gm_out,tile_dat[,"tile_id"],".csv",sep="")

    if(!is.null(fusion_switches)) coms_df=data.frame(paste(gridmetrics[1],fusion_switches),tile_dat[,c("switches","dtm_txt")],heightbreak,cellsize,tile_dat[,"outf"],tile_dat[,"las_txt"])
    if(is.null(fusion_switches)) coms_df=data.frame(gridmetrics[1],tile_dat[,c("switches","dtm_txt")],heightbreak,cellsize,tile_dat[,"outf"],tile_dat[,"las_txt"])

    coms=apply(coms_df,1,paste,collapse=" ")

    for(i in 1:nrow(tile_dat)){
      writeLines(gsub(",","\n",tile_dat[i,"las_file"]),tile_dat[i,"las_txt"])
      writeLines(gsub(",","\n",tile_dat[i,"dtm_file"]),tile_dat[i,"dtm_txt"])
    }

    lapply(coms,shell)

  }

  if(!do_fusion){

    res_i=mapply(gridmetrics
             ,las_files=
             ,dtm_files=
             ,fns=fns
             ,xmin=tile_dat[,c("mnx")],ymin=tile_dat[,c("mny")],xmax=tile_dat[,c("mxx")],ymax=tile_dat[,c("mxy")]
             ,res=cellsize
             ,SIMPLIFY=F
           )
  }

}

# library(lasR)
#
#
# # run_gridmetrics(
# #   tile_project="C:\\Temp\\naip_2015_t1650_p66\\test_project\\intersections.csv"
# #   ,dir_out="C:\\Temp\\naip_2015_t1650_p66\\test_project\\"
# #   )
#
# run_gridmetrics(
#   tile_project="C:\\Temp\\naip_2015_t1650_p66\\test_project\\intersections.csv"
#   ,dir_out="C:\\Temp\\naip_2015_t1650_p66\\test_project\\"
#   ,gridmetrics=c("lasR")
#   )
