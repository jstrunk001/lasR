#'@title
#'  run gridmetrics across a project
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2018-01-28 Header added \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param lasR_project csv file of intersections created by lasR_project() function
#'@param lasR_project_polys shape file of intersections created by lasR_project() function
#'@param dir_out
#'@param n_core
#'@param gridmetrics_type
#'@param heightbreak
#'@param cellsize
#'@param minht
#'@param first
#'@param intensity
#'@param outlier
#'@param fusion_switches
#'@param xmn
#'@param fun
#'@param temp
#'@param fast_cache
#'@param n_cache
#'@param dir_dtm
#'@param dir_las
#'@param skip_existing
#'@param con
#'@param table
#'@param existing_coms
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'
#'  gmi=run_gridmetrics2(
#' lasR_project_poly="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project003.shp"
#' ,dir_out="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_07\\"
#' ,dir_dtm="c:\\usgs_dtms\\dtms\\"
#' ,dir_las="D:\\naip_2015_laz\\"
#' ,n_core=10
#' ,existing_coms="C:\\Temp\\run_gridmetrics\\2018Jan21_152618\\all_commands.txt"
#' ,fast_cache=c(rep("r:\\temp",10),rep("c:\\temp",3),rep("i:\\temp",3),rep(NA,3))
#' ,n_cache=400
#' )
#'

#'
#'@import some_package,some_package2
#'
#'@export
#
#'@seealso \code{\link{lasR_project}}\cr \code{\link{gridmetrics}}\cr




run_gridmetrics2=function(

  lasR_project=NA
  ,lasR_project_polys=NA

  ,dir_out="c:/temp/test_project/gridmetrics"
  ,n_core=4
  ,gridmetrics_type=c("c:\\fusion1\\gridmetrics.exe","lasR")
  ,heightbreak=6
  ,cellsize=66
  ,minht=6
  ,first=T
  ,intensity=F
  ,outlier=c(-5,400)
  ,fusion_switches="/nointensity /first"
  ,xmn=561066,xmx=2805066,ymn=33066,ymx=1551066
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

  ,debug=F

  ,... #additonal arguments to fns

  ){

  options(scipen = 999)

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
  proj_polys@data[,"dtm_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_dtm.txt",sep=""))
  proj_polys@data[,"las_txt"]=backslash(paste(temp,proj_polys@data[,"tile_id"],"_las.txt",sep=""))
  proj_polys@data[,"switches"]=paste("/minht:",minht
                            ," /outlier:",paste(outlier,collapse=",")
                            ," /cellbuffer:2 /gridxy:"
                            ,apply(proj_polys@data[,c("mnx","mny","mxx","mxy")],1,paste,collapse=",")
                            ,sep="")


  if(!is.na(fast_cache[1]) | length(fast_cache)>1){

    proj_polys@data[,"las_file_org"] = proj_polys@data[,"las_file"]
    if(length(fast_cache)==1) proj_polys@data[,"las_file"]=forwardslash(sapply(proj_polys@data[,"las_file_org"],function(x,y) paste(file.path(y,basename(unlist(strsplit(x,",")))),collapse=","),fast_cache))
    if(length(fast_cache)>1){

       #replicate fast cache as many times as there are observations
        v_fast_cache=rep(fast_cache,ceiling(nrow(proj_polys@data)/length(fast_cache)),replace=T)[1:length(proj_polys@data[,"las_file_org"])]
       #rename files
        fn_paths=function(x,y){
          if(is.na(y)) x
          else  paste(file.path(y,basename(unlist(strsplit(x,",")))),collapse=",")
        }
        proj_polys@data[,"las_file"]=forwardslash(mapply(fn_paths,proj_polys@data[,"las_file_org"],v_fast_cache))

      }
  }

  if(!is.null(fusion_switches))
    coms_df=data.frame(gm=paste(gridmetrics_type[1],fusion_switches)
                       ,sw=proj_polys@data[,c("switches")]
                       ,ids=paste("/id:",proj_polys@data[,"tile_id"],sep="")
                       ,dtms=forwardslash(proj_polys@data[,c("dtm_txt")])
                       ,hb=heightbreak
                       ,cs=cellsize
                       ,outf=proj_polys@data[,"outf"]
                       ,las=proj_polys@data[,"las_txt"]
                       )

  if(is.null(fusion_switches))
    coms_df=data.frame(gridmetrics_type[1]
                       ,ids=paste("/id:",proj_polys@data[,"tile_id"],sep="")
                       ,proj_polys@data[,c("switches","dtm_txt")]
                       ,heightbreak
                       ,cellsize
                       ,proj_polys@data[,"outf"]
                       ,proj_polys@data[,"las_txt"]
                       )

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
    if(n_clumps > 1) clumps=cut(sample(1:nrow(proj_polys@data),nrow(proj_polys@data)),n_clumps,labels=F)
    #if(n_clumps > 1) clumps=cut(1:nrow(proj_polys@data),n_clumps,labels=F)
    else clumps=rep(1,nrow(proj_polys@data))

    #prepare for processing
    #iterate through files in clumps
    for(i in 1:n_clumps){

      print(paste("start clump",i,"of",n_clumps,"clumps of",n_cache, "at",Sys.time()))

      this_clump = clumps==i
      next_clump = clumps==i+1

    #copy to fast cache
      if(i == 1 & n_clumps ==1){

        #copy for this iteration
        files_from=unique(unlist(strsplit(proj_polys@data$las_file_org[this_clump],",")))
        files_to=unique(unlist(strsplit(proj_polys@data$las_file[this_clump],",")))
        diff_i = ! files_from == files_to
        file.copy(files_from[diff_i],files_to[diff_i],overwrite = F)

        veci=coms[this_clump]
      }

      if(i == 1 & n_clumps >1){

        #copy for this iteration
        files_from=unique(unlist(strsplit(proj_polys@data$las_file_org[this_clump],",")))
        files_to=unique(unlist(strsplit(proj_polys@data$las_file[this_clump],",")))
        diff_i = ! files_from == files_to
        copy_status=mapply(file.copy,files_from[diff_i],files_to[diff_i],overwrite = F) #otherwise partial copies left with 0kb

        if( sum( !copy_status ) > 0 ){
          #bad_copy=!file.exists(files_to) #better than !copy_status ?
          bad_copy=!copy_status
          copy_status1=mapply(file.copy,files_from[diff_i][bad_copy],files_to[diff_i][bad_copy],overwrite = F)
        }

        #asynchronous copy for next iteration
        files_from=unique(unlist(strsplit(proj_polys@data$las_file_org[next_clump],",")))
        files_to=unique(unlist(strsplit(proj_polys@data$las_file[next_clump],",")))
        diff_i = ! files_from == files_to

        veci=as.list(c(NA,coms[this_clump]))
        veci[[1]]=data.frame(files_from[diff_i],files_to[diff_i],stringsAsFactors = F )

      }
      if(i >1 & i < n_clumps){

        #asynchronous copy for next iteration
        files_from=unique(unlist(strsplit(proj_polys@data$las_file_org[next_clump],",")))
        files_to=unique(unlist(strsplit(proj_polys@data$las_file[next_clump],",")))
        diff_i = ! files_from == files_to
        veci=as.list(c(NA,coms[this_clump]))
        veci[[1]]=data.frame(files_from[diff_i],files_to[diff_i],stringsAsFactors = F)

      }
      if(i > 1 & i==n_clumps){

        veci=coms[this_clump]

      }
if(debug) browser()
      #run process
      res=parLapply( clus ,veci, .fn_copy_shell) ; gc()
      clusterEvalQ(clus,{gc()})
      clusterEvalQ(clus,{ls()})
      gc()

      #delete temporary files - this clump
      files_from=(unlist(strsplit(proj_polys@data$las_file_org[this_clump],",")))
      files_to=(unlist(strsplit(proj_polys@data$las_file[this_clump],",")))
      diff_i= files_from != files_to
      sapply(files_to[diff_i],unlink)

      print(paste("end clump",i,"ofI",n_clumps,"clumps of",n_cache, "at",Sys.time()))

    }
    gc();stopCluster(clus);gc()

  }else{

   lapply(coms,shell) ;gc()

  }
  print("run fusion");print(Sys.time())




}

.fn_copy_shell=function(x){
  if(class(x)=="data.frame"){
    diffs = x[,1] != x[,2]
    file.copy(x[diffs,1],x[diffs,2],overwrite = F)
  }
  else return(shell(x))
}

.do_shell=function(comi,idi,tab_out,emptyi,lock.name){

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

  if(T) library(lasR)

  gmi=run_gridmetrics2(
    lasR_project_poly="D:\\projects\\2017_WA_DSM_Pilot_usgs\\2017Aug_NAIP_usgs\\lasR_project003.shp"
    ,dir_out="I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_07\\"
    ,dir_dtm="c:\\usgs_dtms\\dtms\\"
    ,dir_las="D:\\naip_2015_laz\\"
    ,n_core=23
    #,existing_coms="C:\\Temp\\run_gridmetrics\\2018Jan21_152618\\all_commands.txt"
    ,fast_cache=c(rep("r:\\temp",10),rep("c:\\temp",3),rep("i:\\temp",3),rep(NA,3))
    ,n_cache=300
    ,debug=F
  )


}


if(F){

  272215

  tst = shell("c:\\fusion\\gridmetrics.exe /nointensity /first /minht:6 /outlier:-5,400 /cellbuffer:2 /gridxy:2319966,1349766,2323266,1353066 c:\\temp\\run_gridmetrics\\2018Jan02_152302\\41334_dtm.txt 6 66 I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_02\\gridmetrics_csv\\41334.csv c:\\temp\\run_gridmetrics\\2018Jan02_152302\\41334_las.txt", intern = T);print(tst)

  tst = shell('c:\\fusion\\gridmetrics.exe c:\\temp\\run_gridmetrics\\2018Jan02_152302\\41334_dtm.txt 6 66 I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_02\\gridmetrics_csv\\41334.csv c:\\temp\\run_gridmetrics\\2018Jan02_152302\\41334_las.txt', intern = T);print(tst)

  tst = shell("\"c:/fusion/gridmetrics.exe\" \"c:/temp/run_gridmetrics/2018Jan02_152302/41335_dtm.txt\" 6 66 \"I:/projects/2017_WA_DSM_Pilot/2017Aug_NAIP_usgs/gridmetrics_02/gridmetrics_csv/41335.csv\" \"c:/temp/run_gridmetrics/2018Jan02_152302/41335_las.txt\"", intern = T);print(tst)

  tst = shell("\"c:/fusion/gridmetrics.exe\"", intern = T);print(tst)

  writeClipboard("c:\\fusion\\gridmetrics.exe c:\\temp\\run_gridmetrics\\2018Jan02_152302\\41334_dtm.txt 6 66 I:\\projects\\2017_WA_DSM_Pilot\\2017Aug_NAIP_usgs\\gridmetrics_02\\gridmetrics_csv\\41334.csv c:\\temp\\run_gridmetrics\\2018Jan02_152302\\41334_las.txt")

  x=readLines("C:\\Temp\\run_gridmetrics\\2018Jan21_152618\\all_commands.txt")
  x1=x[grepl(272215,x)]

    }
