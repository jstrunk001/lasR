
run_gridmetrics=function(

  lasR_project=NA
  ,lasR_project_polys=NA


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
  ,fun=compute_metrics2#list(min=min,max=max,mean=mean,sd=sd)#,p20=function(x,...)quantile(x,.2,...),p75=function(x,...)quantile(x,.2,...),p95=function(x,...)quantile(x,.2,...))
  ,temp="c:\\temp\\run_gridmetrics\\"

  ,dir_dtm=NA #in case drive paths are wrong (External drives...)
  ,dir_las=NA #in case drive paths are wrong (External drives...)

  ,skip_existing=T

  ,... #additonal arguments to fns

  ){

  require("parallel")
  require("raster")
  require("rgdal")

  gridmetrics_type=gridmetrics_type[1]
  do_fusion = F
  if(grepl("gridmetrics.exe",gridmetrics_type)) do_fusion = T

  #time stamp for outputs
  proc_time=format(Sys.time(),"%Y%b%d_%H%M%S")

  #create temp folder
  gm_out=backslash(paste(dir_out,"/gridmetrics_csv/",sep=""))
  if(!dir.exists(gm_out)) try(dir.create(gm_out,recursive=T))

  #create csv folder dump
  temp=backslash(paste(temp,"/",proc_time,"/",sep=""))
  if(!dir.exists(temp)) try(dir.create(temp,recursive=T))

 #load lasR_project
  if(!is.na(lasR_project) & is.na(lasR_project_polys[1])){
    if(!inherits(lasR_project,"sp")){
      proj=read.csv(lasR_project)
      proj_polys0=bbox2polys(proj[,c("tile_id","mnx","mxx","mny","mxy")])
      row.names(proj)=proj[,"tile_id"]
      proj_polys=SpatialPolygonsDataFrame(proj_polys0,proj)
    }
    if(inherits(lasR_project,"sp")) proj_polys=lasR_project
  }
  if(!is.na(lasR_project_polys[1])){
    if(!inherits(lasR_project_polys,"sp")) proj_polys=readOGR(dirname(lasR_project_polys),gsub("[.]shp$","",basename(lasR_project_polys)),stringsAsFactors=F)
    if(inherits(lasR_project_polys,"sp")) proj_polys=lasR_project_polys
  }
  print("load lasR_project");print(Sys.time())
  #fix drive paths in lasR_project

  if(!is.na(dir_dtm)) proj_polys@data[,"dtm_file"]=unlist(lapply(as.character(proj_polys@data[,"dtm_file"]),function(...,dir_dtm)paste(file.path(dir_dtm,basename(strsplit(...,",")[[1]])),collapse=","),dir_dtm=dir_dtm))
  if(!is.na(dir_las)) proj_polys@data[,"las_file"]=unlist(lapply(as.character(proj_polys@data[,"las_file"]),function(...,dir_dtm)paste(file.path(dir_dtm,basename(strsplit(...,",")[[1]])),collapse=","),dir_dtm=dir_las))

  #skip existing files
  if(skip_existing){
    files_done=list.files(gm_out,pattern="[.]csv")
    ids_done=gsub("_.*","",files_done)
    files_exist=as.character(proj_polys@data[,"tile_id"]) %in% ids_done
    proj_polys=subset(proj_polys,subset=!files_exist)
  }
  print("skip files");print(Sys.time())

  #prepare output directory
  proj_polys@data[,"outf"]=paste(gm_out,proj_polys@data[,"tile_id"],".csv",sep="")

  #prepare batch commands
  if(do_fusion){

    proj_polys@data[,"dtm_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_dtm.txt",sep=""))
    proj_polys@data[,"las_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_las.txt",sep=""))
    proj_polys@data[,"switches"]=paste("/minht:",minht
                              ," /outlier:",paste(outlier,collapse=",")
                              ," /cellbuffer:2 /gridxy:"
                              ,apply(proj_polys@data[,c("mnx","mny","mxx","mxy")],1,paste,collapse=",")
                              ,sep="")


    if(!is.null(fusion_switches)) coms_df=data.frame(paste(gridmetrics_type[1],fusion_switches),proj_polys@data[,c("switches","dtm_txt")],heightbreak,cellsize,proj_polys@data[,"outf"],proj_polys@data[,"las_txt"])
    if(is.null(fusion_switches)) coms_df=data.frame(gridmetrics_type[1],proj_polys@data[,c("switches","dtm_txt")],heightbreak,cellsize,proj_polys@data[,"outf"],proj_polys@data[,"las_txt"])

    coms=apply(coms_df,1,paste,collapse=" ")

    print("set up commands");print(Sys.time())

    for(i in 1:nrow(proj_polys@data)){
      writeLines(gsub(",","\n",proj_polys@data[i,"las_file"]),proj_polys@data[i,"las_txt"])
      writeLines(gsub(",","\n",proj_polys@data[i,"dtm_file"]),proj_polys@data[i,"dtm_txt"])
    }
    print("create list of dtms and las files");print(Sys.time())

    if(n_core>1){

      clus=makeCluster(n_core)
      clusterEvalQ(clus,{library(lasR);gc()})
      parLapply(clus,coms,shell);gc()
      gc();stopCluster(clus);gc()

    }else{
     lapply(coms,shell);gc()

    }
    print("run fusion");print(Sys.time())
  }

  if(!do_fusion){

    if(n_core>1){
      clus=makeCluster(n_core)
      clusterEvalQ(clus,{library(lasR);gc()})

      res_i=clusterMap(
                      clus
                      ,gridmetrics
                      ,las_files=lapply(proj_polys@data[,"las_file"],function(...)unlist(strsplit(...)),",")
                      ,dtm_files=lapply(proj_polys@data[,"dtm_file"],function(...)unlist(strsplit(...)),",")
                      ,xmin=proj_polys@data[,c("mnx")]
                      ,ymin=proj_polys@data[,c("mny")]
                      ,xmax=proj_polys@data[,c("mxx")]
                      ,ymax=proj_polys@data[,c("mxy")]
                      ,MoreArgs=list(fun=fun,res=cellsize,return=F)
                      ,out_name=proj_polys@data[,"outf"]
                      ,SIMPLIFY=F
                      );gc();stopCluster(clus);gc()

    }

    if(n_core<2){

      res_i=mapply(
                gridmetrics
               ,las_files=lapply(proj_polys@data[,"las_file"],function(...)unlist(strsplit(...)),",")
               ,dtm_files=lapply(proj_polys@data[,"dtm_file"],function(...)unlist(strsplit(...)),",")
               ,xmin=proj_polys@data[,c("mnx")],ymin=proj_polys@data[,c("mny")],xmax=proj_polys@data[,c("mxx")],ymax=proj_polys@data[,c("mxy")]
               ,MoreArgs=list(fun=fun,res=cellsize,return=T)
               ,out_name=proj_polys@data[,"outf"]
               ,SIMPLIFY=F
             );gc()


    }

    return(res_i)
  }

}


if(F){

  library(lasR)

  gmi=run_gridmetrics(
    lasR_project_polys="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_5cnty_lasR\\lasR_project001.shp"
    ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\"
    ,gridmetrics=c("lasR")
    ,n_core=1
    ,dir_dtm="C:\\data\\FUSION_DTMS\\" #in case drive paths are wrong (External drives...)
    ,dir_las="G:\\data\\2015_naip_phodar\\" #in case drive paths are wrong (External drives...)
    ,elev_metrics=F
  )

}
