
run_gridmetrics=function(

  ,lasR_project=NA
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
  ,fns=list(min=min,max=max,mean=mean,sd=sd)#,p20=function(x,...)quantile(x,.2,...),p75=function(x,...)quantile(x,.2,...),p95=function(x,...)quantile(x,.2,...))

  ,dir_dtm=NA #in case drive paths are wrong (External drives...)
  ,dir_las=NA #in case drive paths are wrong (External drives...)

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
  temp=backslash(paste(dirname(lasR_project),"/run_gridmetrics_temp/",proc_time,"/",sep=""))
  if(!dir.exists(temp)) try(dir.create(temp,recursive=T))

  #create sp objects for plots
  polys_in=NULL
  if(!is.na(plot_polys[1])){
    if(inherits(plot_polys,"sp")) polys_in=plot_polys
    if(inherits(plot_polys,"char")) polys_in=readOGR(plot_polys)
  }
  if(!is.na(unlist(idxy)[1]) & !inherits(polys_in,"sp")){
    polys_in=points2polys(idxy)
  }
  if(!is.na(unlist(idxyd)[1]) & !inherits(polys_in,"sp")){

    seq1=c(seq(-pi,pi,.1),pi+.1)
    circle1=data.frame(sin(seq1),cos(seq1))
    spl_idxyd=split(idxyd,1:nrow(idxyd))
    idxy=rbind.fill(lapply(spl_idxyd,function(x,circle1)data.frame(id=x[,1],x=circle1[,1]*x[,4]+x[,2],y=circle1[,2]*x[,4]+x[,3]),circle1))
    row.names(idxyd)=idxyd[,1]

    plot_polys_0=points2polys(idxy)
    plot_polys=SpatialPolygonsDataFrame(plot_polys_0,data=idxyd)

  }
  print("Create Plot Polys");print(Sys.time())

  #clean up self intersections
  proj_polys_b=gBuffer(proj_polys, byid=TRUE, width=0)
  plot_polys_b=gBuffer(plot_polys, byid=TRUE, width=0)

  #create output folder
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

    for(i in 1:nrow(proj)){
      writeLines(gsub(",","\n",proj_polys@data[i,"las_file"]),proj_polys@data[i,"las_txt"])
      writeLines(gsub(",","\n",proj_polys@data[i,"dtm_file"]),proj_polys@data[i,"dtm_txt"])
    }

    lapply(coms,shell);gc()

  }

  if(!do_fusion){

    if(n_core>1){
      clus=makeCluster(n_core)
      clusterEvalQ(clus,{library(lasR);gc()})

      browser()

      res_i=clusterMap(
                    clus
                   ,gridmetrics
                   ,las_files=lapply(proj_polys@data[,"las_file"],function(...)unlist(strsplit(...)),",")
                   ,dtm_files=lapply(proj_polys@data[,"dtm_file"],function(...)unlist(strsplit(...)),",")
                   ,xmin=proj_polys@data[,c("mnx")],ymin=proj_polys@data[,c("mny")],xmax=proj_polys@data[,c("mxx")],ymax=proj_polys@data[,c("mxy")]
                   ,MoreArgs=list(fns=fns,res=cellsize,return=F)
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
               ,MoreArgs=list(fns=fns,res=cellsize,return=T)
               ,out_name=proj_polys@data[,"outf"]
               ,SIMPLIFY=F
             );gc()


    }

    return(res_i)
  }

}


if(F){

  gmi=run_gridmetrics(
    lasR_project="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A\\intersections.csv"
    ,dir_out="C:\\projects\\2017_WA_DSM_Pilot\\DSM_Pilot_A\\"
    ,gridmetrics=c("lasR")
    ,n_core=7
  )

}
