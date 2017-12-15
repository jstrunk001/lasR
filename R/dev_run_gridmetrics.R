
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

  ,fast_cache=NA #preferrably a ram or ssd drive with good parallel read behavior
  ,n_cache=90

  ,dir_dtm=NA #in case drive paths are wrong (External drives...)
  ,dir_las=NA #in case drive paths are wrong (External drives...)

  ,skip_existing=T

  ,con=NA
  ,table="gridmetrics"

  ,existing_coms=c(NA,"C:\\Temp\\run_gridmetrics\\2017Aug17_100740\\all_commands.txt")   #skip setting up new dtm and las files

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
  if(is.na(existing_coms[1])) temp = backslash(paste(temp,"/",proc_time,"/",sep=""))
  if(!is.na(existing_coms[1])) temp = paste(dirname(existing_coms[1]),"/",sep="")
  if(!dir.exists(temp)) try(dir.create(temp,recursive=T))

  coms_out=file.path(temp,"all_commands.txt")

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

  if(!is.na(dir_dtm)) proj_polys@data[,"dtm_file"]=backslash(unlist(lapply(as.character(proj_polys@data[,"dtm_file"]),function(...,dir_dtm)paste(file.path(dir_dtm,basename(strsplit(...,",")[[1]])),collapse=","),dir_dtm=dir_dtm)))
  if(!is.na(dir_las)) proj_polys@data[,"las_file"]=backslash(unlist(lapply(as.character(proj_polys@data[,"las_file"]),function(...,dir_dtm)paste(file.path(dir_dtm,basename(strsplit(...,",")[[1]])),collapse=","),dir_dtm=dir_las)))

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

  print(paste(nrow(proj_polys@data),"tiles to process"))

  #prepare batch commands
  if(do_fusion){

    proj_polys@data[,"dtm_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_dtm.txt",sep=""))
    proj_polys@data[,"las_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_las.txt",sep=""))
    proj_polys@data[,"switches"]=paste("/minht:",minht
                              ," /outlier:",paste(outlier,collapse=",")
                              ," /cellbuffer:2 /gridxy:"
                              ,apply(proj_polys@data[,c("mnx","mny","mxx","mxy")],1,paste,collapse=",")
                              ,sep="")


    if(!is.na(fast_cache)){

      proj_polys@data[,"las_file_org"] = proj_polys@data[,"las_file"]
      proj_polys@data[,"las_file"]=forwardslash(sapply(proj_polys@data[,"las_file_org"],function(x,y) paste(file.path(y,basename(unlist(strsplit(x,",")))),collapse=","),fast_cache))

    }

    if(!is.null(fusion_switches)) coms_df=data.frame(paste(gridmetrics_type[1],fusion_switches),proj_polys@data[,c("switches","dtm_txt")],heightbreak,cellsize,proj_polys@data[,"outf"],proj_polys@data[,"las_txt"])
    if(is.null(fusion_switches)) coms_df=data.frame(gridmetrics_type[1],proj_polys@data[,c("switches","dtm_txt")],heightbreak,cellsize,proj_polys@data[,"outf"],proj_polys@data[,"las_txt"])

    coms=apply(coms_df,1,paste,collapse=" ")

    print("set up commands");print(Sys.time())

    if(is.na(existing_coms[1]) ){

      writeLines(coms,coms_out)

      for(i in 1:nrow(proj_polys@data)){
        writeLines(gsub(",","\n",proj_polys@data[i,"las_file"]),proj_polys@data[i,"las_txt"])
        writeLines(gsub(",","\n",proj_polys@data[i,"dtm_file"]),proj_polys@data[i,"dtm_txt"])
      }
      print("create list of dtms and las files");print(Sys.time())
    }

    if(n_core>1 & is.na(fast_cache)){

      clus=makeCluster(n_core)
      clusterEvalQ(clus,{library(lasR);gc()})
      res=parLapply(clus,coms,shell);gc()
      gc();stopCluster(clus);gc()

    }else if(n_core>1 & !is.na(fast_cache)){

      #set up clusters
      clus=makeCluster(n_core)
      clusterEvalQ(clus,{library(lasR);gc()})

      #figure out number of clumps to make
      n_clumps=ceiling(length(coms)/n_cache)
      clumps=cut(1:nrow(proj_polys@data),n_clumps,labels=F)

      #iterate through files in clumps
      for(i in 1:n_clumps){

        this_clump = clumps==i

        #copy to fast cache
        files_from=unique(unlist(strsplit(proj_polys@data$las_file_org[this_clump],",")))
        files_to=file.path(fast_cache,basename(files_from))
        file.copy(files_from, files_to)

        #run clump of commands
        res=parLapply(clus,coms[this_clump],shell);gc()

        #delete temporary files
        unlink(files_to)

      }
      gc();stopCluster(clus);gc()

    }else{

     lapply(coms,shell) ;gc()

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

do_shell=function(comi,idi,tab_out,emptyi,lock.name){

  #test for completion status &
  #create an empty file to denote that processing has not completed, then unlink the empty file
  if(file.exists(emptyi)){
    unlink(list.files(basename(emptyi),pattern=paste(idi)))
    #clean records from database too
  }
  file(emptyi)
  shell(comi)

  dati=read.csv(list.files(pattern=paste(idi,".*[.]csv$",sep=""))[1])

  #lock and write
  ll = lock(lock.name)
  dbWriteTable(db,tab_out,dati)
  unlock(ll)

  #
  unlink(emptyi)

}


if(F){

  if(F) library(lasR)

  gmi=run_gridmetrics(
    lasR_project_poly="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project001.shp"
    ,dir_out="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_01\\"
    ,dir_dtm="r:\\usgs_dtms\\dtm_tiles"
    ,dir_las="D:\\naip_2015_laz\\"
    ,n_core=20
    ,existing_coms="C:\\Temp\\run_gridmetrics\\2017Dec15_130319\\all_commands.txt"
    ,fast_cache="r:\\temp"
    ,n_cache=200
  )


}

