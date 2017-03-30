
run_gridmetrics=function(

  tile_project="c:/temp/test_project/intersections.csv"
  ,dir_out="c:/temp/test_project/gridmetrics"
  ,n_core=4
  ,gridmetrics_type=c("c:\\fusion\\gridmetrics.exe","lasR")
  ,heightbreak=6
  ,cellsize=66
  ,minht=6
  ,first=T
  ,intensity=F
  ,outlier=c(-5,400)
  ,fusion_switches="/nointensity /first"
  ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
  #,fns=list(min=min,max=max,mean=mean,sd=sd,p20=function(x,...)quantile(x,.2,...),p75=function(x,...)quantile(x,.2,...),p95=function(x,...)quantile(x,.2,...))
  ,fns=list(min=min,max=max,mean=mean,sd=sd)#,p20=function(x,...)quantile(x,.2,...),p75=function(x,...)quantile(x,.2,...),p95=function(x,...)quantile(x,.2,...))

  ){
  require("parallel")
  require("raster")

  gridmetrics_type=gridmetrics_type[1]
  do_fusion = F
  if(grepl("gridmetrics.exe",gridmetrics_type)) do_fusion = T

  #time stamp for outputs
  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")

  #create temp folder
  gm_out=backslash(paste(dir_out,"/gridmetrics_csv/",proc_time,"/",sep=""))
  if(!dir.exists(gm_out)) try(dir.create(gm_out,recursive=T))

  #create csv folder dum
  temp=backslash(paste(dirname(tile_project),"/run_gridmetrics_temp/",proc_time,"/",sep=""))
  if(!dir.exists(temp)) try(dir.create(temp,recursive=T))

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

    if(!is.null(fusion_switches)) coms_df=data.frame(paste(gridmetrics_type[1],fusion_switches),tile_dat[,c("switches","dtm_txt")],heightbreak,cellsize,tile_dat[,"outf"],tile_dat[,"las_txt"])
    if(is.null(fusion_switches)) coms_df=data.frame(gridmetrics_type[1],tile_dat[,c("switches","dtm_txt")],heightbreak,cellsize,tile_dat[,"outf"],tile_dat[,"las_txt"])

    coms=apply(coms_df,1,paste,collapse=" ")

    for(i in 1:nrow(tile_dat)){
      writeLines(gsub(",","\n",tile_dat[i,"las_file"]),tile_dat[i,"las_txt"])
      writeLines(gsub(",","\n",tile_dat[i,"dtm_file"]),tile_dat[i,"dtm_txt"])
    }

    lapply(coms,shell);gc()

  }

  if(!do_fusion){

    if(n_core>1){
      clus=makeCluster(n_core)
      clusterEvalQ(clus,{library(lasR)})
      res_i=clusterMap(
                    clus
                   ,function(...)try(gridmetrics(...))
                   ,las_files=lapply(tile_dat[,"las_file"],function(...)unlist(strsplit(...)),",")
                   ,dtm_files=lapply(tile_dat[,"dtm_file"],function(...)unlist(strsplit(...)),",")
                   ,xmin=tile_dat[,c("mnx")],ymin=tile_dat[,c("mny")],xmax=tile_dat[,c("mxx")],ymax=tile_dat[,c("mxy")]
                   ,MoreArgs=list(fns=fns,res=cellsize)
                   #,out_name=??
                   ,SIMPLIFY=F
                  );gc()
    }

    if(n_core<2){
#
#       browser()
#       tile_dat0=tile_dat
#       tile_dat=tile_dat[7:9,]

      res_i=mapply(
                function(...)try(gridmetrics(...))
               ,las_files=lapply(tile_dat[,"las_file"],function(...)unlist(strsplit(...)),",")
               ,dtm_files=lapply(tile_dat[,"dtm_file"],function(...)unlist(strsplit(...)),",")
               ,xmin=tile_dat[,c("mnx")],ymin=tile_dat[,c("mny")],xmax=tile_dat[,c("mxx")],ymax=tile_dat[,c("mxy")]
               ,MoreArgs=list(fns=fns,res=cellsize,return=T)
               #,out_name=??
               ,SIMPLIFY=F
             );gc()
#
#       j=1
#       gridmetrics(las_files=unlist(lapply(tile_dat[j,"las_file"],function(...)unlist(strsplit(...)),","))
#                   ,dtm_files=unlist(lapply(tile_dat[j,"dtm_file"],function(...)unlist(strsplit(...)),","))
#                   ,xmin=tile_dat[j,c("mnx")],ymin=tile_dat[j,c("mny")],xmax=tile_dat[j,c("mxx")],ymax=tile_dat[j,c("mxy")]
#
#       )

    }

    return(res_i)
  }

}

 #library(lasR)
#
#
# # run_gridmetrics(
# #   tile_project="C:\\Temp\\naip_2015_t1650_p66\\test_project\\intersections.csv"
# #   ,dir_out="C:\\Temp\\naip_2015_t1650_p66\\test_project\\"
# #   )
#
if(F){

  gmi=run_gridmetrics(
    tile_project="C:\\Temp\\naip_2015_t1650_p66\\test_project\\intersections.csv"
    ,dir_out="C:\\Temp\\naip_2015_t1650_p66\\test_project\\"
    ,gridmetrics=c("lasR")
    ,n_core=1
    )

}
