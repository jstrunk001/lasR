#functions to perform estimation

estimate=function(
  x
  ,resp_nm="volcfgrs"
  ,aux_nm="ht_p70"
  ,strata_nm="str"
  ,wt_nm=c(NA,"ef")
  ,ef_nm=c(NA,"ef")      # 1/wt
  ,su_nm=c(NA,"su") #sampling unit for two-stage / multi-stage
  ,reg_form= "ht_p20 + ht_p70 * percentage_first_returns_above_6_00"
  ,pop
  ,N
  ,strata_cuts
  ,type=c("ht","regression","stratified","two-stage","calibrate")
  ,var_type=c("asym","bs","svypkg")
){

  if(is.na(wt_nm) & !is.na(ef_nm)){
    wt_nm="wt"
    x[,wt_nm] = 1 / x[,ef_nm]
  }
  if(is.na(wt_nm) & is.na(ef_nm)){
    wt_nm="wt"
    x[,wt_nm] = nrow(x) / N
  }
  warning("Multi-stage Not Yet Implemented")

  if(length(resp_nm) > 1){stop("1 response at this time")}

  #always compute ht-srs
  res0=.ht(x=x,resp_nm=resp_nm,wt_nm=wt_nm[1],ef_nm=ef_nm[1],N=N,var_type=var_type[1])

  #compute estimates
  if(type[1]=="ht"){
    res=.ht(x, resp_nm=resp_nm, wt_nm=wt_nm[1], ef_nm=ef_nm[1], N=N, var_type=var_type[1], type=type[1])
  }
  if(type[1]=="regression"){
    res=.reg(x, resp_nm=resp_nm, wt_nm=wt_nm[1], reg_form=reg_form, ef_nm=ef_nm[1], pop=pop, N=N, var_type=var_type[1], type=type[1])
  }
  if(type[1]=="stratified"){
    res=.str(x,resp_nm,aux_nm,wt_nm,ef_name,pop,N,strata_cuts,var_type[1])
  }
  if(type[1]=="two-stage"){
    res=.ts(x,resp_nm,aux_nm,wt_nm,ef_name,pop,N,strata_cuts,var_type[1])
  }
  if(type[1]=="multi-stage"){
    res=.ms(x,resp_nm,aux_nm,wt_nm,ef_name,pop,N,strata_cuts,var_type[1])
  }
  if(type[1]=="calibrate"){
    res=.cb(x,resp_nm,aux_nm,wt_nm,ef_name,pop,N,strata_cuts,var_type[1])
  }

  return(.summary(res,res0))

}

#generate data to test various functions
pop_test=function(
  N=10000
  ,n=200
  ,wt=c("equal","rand","propx","propy")
  ,err=c("homoskedastic","heteroskedastic")
  ,nstrat=10
  ,nclus=10
  ,type=c("random","regression","stratified","two-stage")
){


  x_in=NULL
  str_in = NULL

  if(type[1]=="random"){

    x_in=abs(rnorm(N))*100
    y_in=x_in + rnorm(N)*100/5

    if(wt[1]=="equal") p_i= rep(1/N,N)
    if(wt[1]=="propx") p_i= 1/(2 - (x_in-min(x_in)+quantile(abs(x_in),.1))/diff(range(x_in)))
    if(wt[1]=="propy") p_i= 1/(2 - (y_in-min(y_in)+quantile(abs(y_in),.1))/diff(range(y_in)))

    p_i=p_i/sum(p_i)
    wti=1/p_i

    s_ix=sample(N,n,prob=p_i)

  }
  if(type[1]=="regression"){

    x_in=abs(rnorm(N))*100
    if(err[1] == "homoskedastic") y_in=abs(x_in + rnorm(N)*100/5)
    if(err[1] == "heteroskedastic") y_in=abs(x_in + rnorm(N)*100/15 + rnorm(N)*x_in/4)

    if(wt[1]=="equal") p_i= rep(1/N,N)
    if(wt[1]=="propx") p_i= 1/(2 - (x_in-min(x_in)+quantile(abs(x_in),.1))/diff(range(x_in)))
    if(wt[1]=="propy") p_i= 1/(2 - (y_in-min(y_in)+quantile(abs(y_in),.1))/diff(range(y_in)))

    p_i=p_i/sum(p_i)
    wti=1/p_i

    s_ix=sample(N,n,prob=p_i)
    #s_in=data.frame(dat_pop , wt=wti,pi=p_i)[s_ix,]

  }
  if(type[1]=="stratified"){

    x_in=abs(rnorm(N))*100
    if(err[1] == "homoskedastic") y_in=abs(x_in + rnorm(N)*100/5)
    if(err[1] == "heteroskedastic") y_in=abs(x_in + rnorm(N)*100/15 + rnorm(N)*x_in/4)
    cut_vals_in=quantile(x_in,(0:nstrat)/nstrat)
    str_in=cut(x_in,cut_vals_in, labels = FALSE, include.lowest = T)
    p_i=1/N
    wti=1/p_i
    spl_pop_ix=split(1:N,str_in)
    s_cut=cut(1:n,nstrat,labels = FALSE)
    s_spl=split(s_cut,s_cut)
    si_n=sapply(s_spl,length)

    #adjust weights to sample proportional to size
    if(wt=="propx"){
      spl_pop_x=split(x_in,str_in)
      pop_Ni=sapply(spl_pop_x,length)
      pop_mui=sapply(spl_pop_x,mean)

      wti_n= pop_mui*(si_n / pop_Ni)/sum(pop_mui) * nstrat
      si_n=round(pop_Ni*wti_n)
      si_n[si_n<2] = 2
      #force sample size to n - remove observations from largest group
      if(sum(si_n) > n) si_n[length(si_n)] = si_n[length(si_n)] - (sum(si_n) - n)

      p_i=(si_n/pop_Ni)[str_in]
      p_i=p_i/sum(p_i)
      wti = 1 / p_i

    }
    if(wt=="propy"){

      spl_pop_y=split(y_in,str_in)
      pop_Ni=sapply(spl_pop_y,length)
      pop_mui=sapply(spl_pop_y,mean)
      wti= pop_mui*(si_n / pop_Ni)/sum(pop_mui) * nstrat
      si_n=round(pop_Ni*wti)
      si_n[si_n<2] = 2
      #force sample size to n - remove observations from largest group
      if(sum(si_n) > n) si_n[length(si_n)] = si_n[length(si_n)] - (sum(si_n) - n)

      p_i=si_n/pop_Ni
      p_i=pi/sum(p_i)
      wti = 1 / p_i

    }

    s_ix=try(unlist(mapply(sample,spl_pop_ix,si_n,replace=F,SIMPLIFY = F)),silent=T)
    if(class(s_ix)=="try-error") s_ix=unlist(mapply(sample,spl_pop_ix,si_n,replace=T,SIMPLIFY = F))

  }
  if(type[1]=="two-stage"){

    #res=list(type=type[1],pop=dat_pop,s=s_in,N=N,n=n,wt=wt[1],nstrat=nstrat,nclus=nclus)

  }
  if(type[1]=="multi-stage"){

    #res=list(type=type[1],pop=dat_pop,s=s_in,N=N,n=n,wt=wt[1],nstrat=nstrat,nclus=nclus)

  }

#create population
  dat_pop=data.frame(y=y_in)
  dat_pop[,"x"]=x_in
  dat_pop[,"str"]=str_in
  dat_pop[,"wt"]=wti
  dat_pop[,"pi"]=p_i

  s_in=dat_pop[s_ix,]

  res=list(type=type[1],pop=dat_pop,s=s_in,N=N,n=n,wt=wt[1],nstrat=1,nclus=1)

  return(res)

}

if(F){

  #generate various populations
  p1=pop_test(type=c("random"),wt="equal")
  p2=pop_test(type=c("random"),wt="propx")
  p3=pop_test(type=c("random"),wt="propy")
  p4=pop_test(type=c("regression"),wt="equal")
  p5=pop_test(type=c("regression"),wt="propx")
  p6=pop_test(type=c("regression"),wt="propy")
  p7=pop_test(type=c("regression"),wt="equal",err="heteroskedastic")
  p8=pop_test(type=c("regression"),wt="propx",err="heteroskedastic")
  p9=pop_test(type=c("regression"),wt="propy",err="heteroskedastic")
  p10=pop_test(type=c("stratified"),wt="equal")
  p11=pop_test(type=c("stratified"),wt="propx")
  p12=pop_test(type=c("stratified"),wt="propy")
  p13=pop_test(type=c("stratified"),wt="equal",err="heteroskedastic")
  p14=pop_test(type=c("stratified"),wt="propx",err="heteroskedastic")
  p15=pop_test(type=c("stratified"),wt="propy",err="heteroskedastic")

  #plot resulting data
  plot(density(p1$pop$y),col="blue","simple random sample");lines(density(p1$s$y),col="red");legend("topright",legend=c("population","sample"),lty=1,col=c("blue","red"))
  plot(density(p2$pop$y),col="blue","random sample, wti ~ some x");lines(density(p2$s$y),col="red");lines(density(p2$s$y,weights=p2$s$wt/sum(p2$s$wt)),col="green");legend("topright",legend=c("population","sample","weighted sample"),lty=1,col=c("blue","red","green"))
  plot(density(p3$pop$y),col="blue" ,"random sample wti ~  y");lines(density(p3$s$y),col="red");lines(density(p3$s$y,weights=p3$s$wt/sum(p3$s$wt)),col="green");legend("topright",legend=c("population","sample","weighted sample"),lty=1,col=c("blue","red","green"))

  plot(p4$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="simple random sample, wti ~ 1 , homoskedastic");points(p4$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p5$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample, wti ~ some x, homoskedastic");points(p5$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p6$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample wti ~  y , homoskedastic");points(p6$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

  plot(p7$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="simple random sample, wti ~ 1 , heteroskedastic");points(p7$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p8$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample, wti ~ some x, heteroskedastic");points(p8$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p9$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample wti ~  y , heteroskedastic");points(p9$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

  plot(p10$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ 1 , homoskedastic");points(p10$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p11$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ some x , homoskedastic");points(p11$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p12$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ y , homoskedastic");points(p12$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

  plot(p13$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ 1 , heteroskedastic");points(p13$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p14$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ some x , heteroskedastic");points(p14$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p15$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ y , heteroskedastic");points(p15$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

}

#random sample
if(F){

  p1=pop_test(type=c("random"),wt="equal")
  p2=pop_test(type=c("random"),wt="propx")
  p3=pop_test(type=c("random"),wt="propy")

  ei=rbind(
   # estimate(x=p1$s,resp_nm="y",wt_nm="wt",N=nrow(p1$pop),var_type="asym")
  estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="svypkg")
  ,estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="asym")
  ,estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="bs")
  )
  ei


  e_avg=apply(ei[,-c(1:7)],2,mean)
  round(e_avg,3)
  sqrt(var(p3$pop$y * p3$pop$wt ) / nrow(p3$s)) / nrow(p3$pop)

  #c(mean(p1$pop$y),mean(p2$pop$y),mean(p3$pop$y))
  #sd(p2$pop$y)/sqrt(nrow(p2$s))

}

#sample y with covariate x
if(F){

  p6=pop_test(type=c("regression"),wt="propy",n=500)

  e6a=estimate(x=p6$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p4$pop$x)) ,N=nrow(p6$pop),type="regression" ,var_type="asym")
  e6b=estimate(x=p6$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p4$pop$x)) ,N=nrow(p6$pop),type="regression" ,var_type="svypkg")
  e6c=estimate(x=p6$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p4$pop$x)) ,N=nrow(p6$pop),type="regression" ,var_type="bs")

  rbind(e6a,e6b,e6c)

  c(reg_mn=e6a$mean, pop_mn=mean(p6$pop$y),  diff_mn = mean(p6$pop$y) - e6a$mean)
  c(reg_mn=e6b$mean, pop_mn=mean(p6$pop$y),  diff_mn = mean(p6$pop$y) - e6b$mean)
}

#stratification
if(F){


  }

.summary=function(res,res0){

  #computations
  rsq_in = 1 - res$se_m^2 / res0$se_m^2
  deff_in = res$se_m^2 / res0$se_m^2
  deffinv_in = res0$se_m^2 / res$se_m^2

  #return results
  data.frame(

    type=res$type
    ,resp=res$resp_nm
    ,var_type=res$var_type
    ,wt_nm=res$wt_nm
    ,ef_nm=res$ef_nm
    ,su_nm=res$su_nm
    ,formula=res$formula
    ,mean=res$mean
    ,total=res$total
    ,se_m=res$se_m
    ,se_m_srs=res0$se_m
    ,se_t=res$se_t
    ,se_t_srs=res0$se_t
    ,rmse=res$rmse
    ,rmse_srs=res0$rmse
    ,rsq = rsq_in
    ,deff = deff_in
    ,deffinv = deffinv_in
    ,n_str=res$n_str
    ,n_clus=res$n_clus

  )
}

#
.ht=function(x,resp_nm,wt_nm,ef_nm,pop,N,type,var_type,n_bs=400){

  require(survey)
  n=nrow(x)
  t_rs = sum(x[,resp_nm] * x[,wt_nm] ) / n
  mn_rs = t_rs / N

  if(var_type[1]=="asym"){
    #hansen-herwitz variance estimator

    v_t_rs = var(x[,resp_nm] * x[,wt_nm] ) / n
  }

  if(var_type[1]=="bs"){

    fn_bs=function(i,x,resp_nm,wt_nm){
      n=nrow(x)
      bs_ix=sample(nrow(x),prob=x[,wt_nm],replace=T)
     # bs_ix=sample(nrow(x),replace=T)
      sum(x[bs_ix,resp_nm]*x[bs_ix,wt_nm]) / n
      #N*mean(x[bs_ix,resp_nm])
    }

    v_t_rs = var(sapply(1:n_bs,fn_bs,x,resp_nm,wt_nm))
  }
  if(var_type[1]=="svypkg"){

    svy_srs = svydesign(ids=~1, data=x, weights = x[, wt_nm] )
    form_svy=as.formula(paste(resp_nm, " ~ 1" ))
    t_svy=data.frame(svytotal(form_svy,svy_srs))/n
    v_t_rs = t_svy[,2]^2
    t_rs = t_svy[,1]


  }

  se_t_rs = sqrt(v_t_rs)
  se_m_rs = se_t_rs / N
  rmse_rs = se_m_rs * sqrt(n)
  mn_rs = t_rs / N

  return(
    list(type = "ht-srs"
         ,resp_nm = resp_nm
         ,var_type = var_type
         ,wt_nm = wt_nm
         ,ef_nm = ef_nm
         ,su_nm = NA
         ,formula = NA
         ,mean = mn_rs
         ,total = t_rs
         ,se_m = se_m_rs
         ,se_t = se_t_rs
         ,rmse = rmse_rs
         ,n = n
         ,N = N
         ,n_str=NA
         ,n_clus=NA
    )
  )
}

.reg=function(x,resp_nm,wt_nm,ef_nm,reg_form,pop,N,type,var_type){

  n = nrow(x)
  pop_mn=pop/N

  if(F){

    svy_rs=svydesign(id=~1,data=x)
    svy_glm=svyglm(reg_form,svy_rs)
    t_svglm=predict(svy_glm,newdata=pop,total=N)
    m_svglm=predict(svy_glm,newdata=pop/N)

  }

  if(var_type[1] ==  "asym"){

    lm_in=lm(reg_form, data=x)
    lm_summ=summary(lm_in)

    t_e = sum( residuals(lm_in) * x[, wt_nm] ) /  n
    t_reg = N * predict(lm_in, newdata = pop / N ) + t_e

    #hansen hurwitz
    v_t_reg = sum((fitted(lm_in) * x[, wt_nm] - x[, resp_nm]* x[, wt_nm] )^2 / ( nrow(x) - 1 ) )/ n

  }
  if(var_type[1] ==  "bs"){

    # .632 bootstrap prediction error - unweighted
    lm_in=lm(reg_form, data=x)

    t_e = sum( residuals(lm_in) * x[, wt_nm] ) /  n
    t_reg = N * predict(lm_in, newdata = pop / N ) + t_e

    lm_in$call = call("lm",formula=as.formula(lm_in),data=x,x=T)
    bs_res=lm_boot(model=lm_in,n_boot = 500)
    v_t_reg = (N*bs_res$err_632)^2 / n

  }
  if(var_type[1] ==  "svypkg"){

    require(survey)

    pop_svy=c("(Intercept)" = N  ,unlist(pop) )

    svy_wts=as.formula(paste("~",wt_nm))
    svy_srs = svydesign(ids=~1,weights=svy_wts,data=x)
    svy_reg = svyglm(reg_form,svy_srs)
    cb1 = calibrate(svy_srs,reg_form,population=pop_svy,deff=T)
    cb_form = as.formula(paste("~",resp_nm))
    t_cb = data.frame(svytotal(cb_form,cb1))
    t_reg = t_cb[,1]

    v_t_reg = t_cb[,2]^2


  }

  mn_reg = t_reg / N
  se_t_reg = sqrt(v_t_reg)
  se_m_reg = se_t_reg / N
  rmse_reg = se_m_reg * sqrt(n)

  return(
    list(type = type[1]
         ,resp_nm = resp_nm
         ,var_type = var_type
         ,wt_nm = wt_nm
         ,ef_nm = ef_nm
         ,su_nm = NA
         ,formula = NA
         ,mean = mn_reg
         ,total = t_reg
         ,se_m = se_m_reg
         ,se_t = se_t_reg
         ,rmse = rmse_reg
         ,n = n
         ,N = N
         ,n_str=NA
         ,n_clus=NA
    )
  )

}


.str=function(x,resp_nm,strata_nm,wt_nm,ef_name,pop,N,var_type){

  form1 = as.formula(paste(resp_nm,"~",strata_nm))
  ni = aggregate(form1, data=x, FUN=length )
  names(ni)[2]="ni"

  if(var_type[1] == "asym"){

    varsi = aggregate(form1,data=x,FUN=var )
    sdi = aggregate(form1,data=x,FUN=sd )
    mnsi = aggregate(form1,data=x,FUN=mean )
    sst = aggregate(form1,data=x,FUN=function(x,...) c(sst=var(x)*(length(x)-1)) )

    #fix cases of too few observations to compute strata variance
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

    v_t_str = 1 / sum(varsib[,aux_nm])^2 * sum(varsib[,resp_nm] * varsib[,aux_nm]^2 / (varsib[,"ni"]) )

  }

  if(var_type[1] == "svypkg"){

  }

  if(var_type[1] == "bs"){

  }

  n=sum(ni[,"ni"])
  se_t_str = sqrt(v_t_str)
  se_m_str = se_t_str / N
  rmse_str = se_m_str * sqrt(n)
  mn_str = sum(mnsib[,resp_nm]*mnsib[,aux_nm])/sum(varsib[,aux_nm])
  t_str = mn_str * N

  return(
    list(type = "stratified"
         ,resp_nm = resp_nm
         ,var_type = var_type
         ,wt_nm = wt_nm
         ,ef_nm = ef_nm
         ,su_nm = NA
         ,formula = form_str
         ,mean = mn_str
         ,total = t_str
         ,se_m = se_m_str
         ,se_t = se_t_str
         ,rmse = rmse_str
         ,n = n_str
         ,N = N
         ,n_str=nrow(ni)
         ,n_clus=NA
    )
  )

}
.str_old=function(x,resp_nm,aux_nm,strata_nm,pop){

  form1 = as.formula(paste(resp_nm,"~",strata_nm))
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

.str_cut=function(x,aux_nm,strata_cuts){

  as.character(cut(x[,aux_nm],strata_cuts,labels=F))

}

.cb=function(x,resp_nm,wt_nm,ef_name,reg_form,pop,N,type,var_type){


}

.ts=function(...){



}

.ms=function(...){

  stop("Multi-stage Not Yet Implemented")

}
