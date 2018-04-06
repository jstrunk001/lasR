#functions to perform estimation

estimate=function(x,resp_nm,aux_nm,strata_nm,wt_nm,pop,strata_cuts,type=c("reg","srs","pstr","2stg"),var_type=c("asym","bs","svypkg")){
  
  
  if(type=="srs"){
  }
  if(type=="reg"){
  }  
  if(type=="pstr"){
  }  
  if(type=="2stg"){
  }
  if(type==""){
  }
  
  
}

#


#extract core functionality
.ps=function(x,resp_nm,aux_nm,strata_nm,pop){
  
  form1 = as.formula(paste(resp_nm,"~",strata_nm))
  ni = aggregate(form1, data=x, FUN=length )
  ni = aggregate(form1, data=x, FUN=length )
  names(ni)[2]="ni"
  varsi = aggregate(form1,data=x,FUN=var )
  sdi = aggregate(form1,data=x,FUN=sd )
  mnsi = aggregate(form1,data=x,FUN=mean )
  sst = aggregate(form1,data=x,FUN=function(x,...) c(sst=var(x)*(length(x)-1)) )
  
  if(sum(ni[,"ni"]<2)>0){
    
    has_one=which(ni[,"ni"]<2)
    has_more=which(ni[,"ni"]>2)
    
    for(i in 1:length(has_one)){
      varsi[has_one[i],resp_nm] = varsi[has_more[which.min(abs(has_one-has_more))],resp_nm]
    }
    
  }
  varsib = merge(merge(pop,varsi,by=strata_nm),ni,by=strata_nm)
  varsib$sst = varsib[, resp_nm ] * (varsib[, "ni" ] - 1) 
  mnsib = merge(merge(pop,mnsi,by=strata_nm),ni,by=strata_nm)
  
  var1 = 1 / sum(varsib[,aux_nm])^2 * sum(varsib[,resp_nm] * varsib[,aux_nm]^2 / (varsib[,"ni"]) ) 
  var2=(var(x[,resp_nm])/nrow(x))
  
  se1=sqrt(var1)
  se2=sqrt(var2)
  rsq=1-var1/var2
  deff1=var1/var2
  deinv=1/deff1
  mn_pstr=sum(mnsib[,resp_nm]*mnsib[,aux_nm])/sum(varsib[,aux_nm])
  
  list(var_ps=var1,var_srs=var2,se_ps=se1,se_srs=se2,rsq_ps=rsq,deff_ps=deff1,deinv_ps=deinv,mn_pstr=mn_pstr)
  
}

.cut=function(x,aux_nm,strata_cuts){
  
  as.character(cut(x[,aux_nm],strata_cuts,labels=F))
  
}