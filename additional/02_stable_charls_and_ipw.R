suppressPackageStartupMessages({library(survival);library(dplyr);library(splines)})
args<-commandArgs(trailingOnly=TRUE);root<-args[1];out<-args[2]
set.seed(20261011)
write_out<-function(x,name)write.csv(x,file.path(out,name),row.names=FALSE,na='')
pw<-readRDS(file.path(root,'data/derived/charls_person_wave_scored.rds'))
cat('Education labels in source file\n')
if(requireNamespace('haven',quietly=TRUE)){
  z<-haven::read_dta(file.path(root,'data/CHARLS/Harmonized/H_CHARLS_D_Data.dta'),n_max=1,col_select='raeduc_c')
  print(attr(z$raeduc_c,'labels'))
}
a<-readRDS(file.path(root,'data/derived/charls_multistate_analytic.rds'));a$id_cluster<-a$ID
t<-readRDS(file.path(root,'data/derived/charls_transition_intervals_scored.rds'));t$id_cluster<-t$ID
lag<-pw[,c('ID','wave','adapted_hbcs_18')];names(lag)[3]<-'lagged_hbcs';lag$wave<-lag$wave+1L
lagdata<-left_join(t,lag,by=c('ID','wave'));lagdata<-lagdata[!is.na(lagdata$age)&lagdata$age>=50&lagdata$state==0,]
cat('Events by sparse original education level in lagged sample\n')
print(with(lagdata[complete.cases(lagdata[,c('lagged_hbcs','age','sex','education')]),],table(education,transition)))

# Keep every original education level while removing the need to estimate
# coefficients for levels with no events in a given sparse sensitivity sample.
fit_one<-function(d,trans,label,score='adapted_hbcs_18',weight=NULL,need_se=TRUE){
  d<-d[d$state==0&complete.cases(d[,c(score,'age','sex','education','interval_years')]),]
  d$hbcs_per3<-d[[score]]/3;d$event<-as.integer(d$transition==trans)
  form<-Surv(rep(0,nrow(d)),interval_years,event)~hbcs_per3+age+factor(sex)+strata(wave,education)+cluster(id_cluster)
  fit<-if(is.null(weight))coxph(form,data=d,ties='efron') else coxph(form,data=d,weights=d[[weight]],ties='efron')
  if(anyNA(coef(fit))||any(!is.finite(coef(fit))))stop('Unstable coefficient')
  s<-summary(fit);co<-s$coefficients['hbcs_per3',];ci<-s$conf.int['hbcs_per3',]
  row<-data.frame(cohort='CHARLS',transition=trans,analysis=label,participants=n_distinct(d$id_cluster),intervals=nrow(d),events=sum(d$event),HR=ci['exp(coef)'],lower95=ci['lower .95'],upper95=ci['upper .95'],p=co['Pr(>|z|)'],beta=co['coef'],robust_se=co['robust se'],education_adjustment='Joint wave and original education strata')
  rownames(row)<-NULL;list(fit=fit,row=row)
}
rows<-list()
for(tr in c('0_to_1_stroke','0_to_2_death')){
  rows[[length(rows)+1]]<-fit_one(lagdata,tr,'Previous-wave score',score='lagged_hbcs')$row
  rows[[length(rows)+1]]<-fit_one(a[a$wave==3&a$next_wave==4,],tr,'CHARLS 2015-2018 only')$row
}
write_out(bind_rows(rows),'charls_stable_sensitivity_models.csv')
target<-readRDS(file.path(out,'ipw_target_for_bootstrap.rds'))
formula_sel<-observed~ns(age,3)+factor(sex)+factor(education)+factor(wave)+ns(bmi,3)+factor(score_smoking)+factor(score_alcohol)+factor(score_hypertension)+factor(score_diabetes)+factor(score_sleep)+factor(score_mood)+partner+event_type
weight_data<-function(d){
  mod<-glm(formula_sel,data=d,family=binomial())
  if(!mod$converged||anyNA(coef(mod)))stop('Selection model convergence failure')
  prob<-predict(mod,type='response')
  if(any(!is.finite(prob)|prob<=0|prob>=1))stop('Positivity failure')
  d$p_observed<-prob;prevalence<-mean(d$observed)
  d<-d[d$observed==1,];d$ipw_untruncated<-prevalence/d$p_observed
  q<-quantile(d$ipw_untruncated,c(.01,.99));d$ipw_truncated<-pmin(q[2],pmax(q[1],d$ipw_untruncated));d
}
w<-weight_data(target)
trnames<-c('0_to_1_stroke','0_to_2_death')
res<-list()
for(tr in trnames)for(col in c('ipw_truncated','ipw_untruncated')){
  res[[length(res)+1]]<-fit_one(w,tr,if(col=='ipw_truncated')'Activity-selection IPW, 1st/99th percentile truncation' else 'Activity-selection IPW, untruncated',weight=col)$row
}
ans<-bind_rows(res)
ids<-unique(target$id_cluster);index<-split(seq_len(nrow(target)),target$id_cluster)
B<-200L;betas<-matrix(NA_real_,B,4)
for(b in seq_len(B)){
  draw<-sample(ids,length(ids),replace=TRUE)
  pieces<-lapply(seq_along(draw),function(i){z<-target[index[[draw[i]]],];z$id_cluster<-paste0('boot_',i);z})
  boot<-bind_rows(pieces)
  value<-tryCatch({
    bw<-weight_data(boot);v<-numeric(4);k<-0L
    for(tr in trnames)for(col in c('ipw_truncated','ipw_untruncated')){k<-k+1L;v[k]<-coef(fit_one(bw,tr,'bootstrap',weight=col)$fit)['hbcs_per3']}
    v
  },error=function(e){cat('Bootstrap failed ',b,': ',conditionMessage(e),'\n',sep='');rep(NA_real_,4)})
  betas[b,]<-value
  if(b%%20==0)cat('IPW participant bootstrap ',b,'/',B,'\n',sep='')
}
for(k in 1:4){
  v<-betas[,k];nvalid<-sum(is.finite(v))
  if(nvalid<.95*B)stop('Too many bootstrap failures; do not publish IPW CI')
  se<-sd(v,na.rm=TRUE);ans$bootstrap_se[k]<-se;ans$bootstrap_valid[k]<-nvalid
  ans$lower95[k]<-exp(ans$beta[k]-1.96*se);ans$upper95[k]<-exp(ans$beta[k]+1.96*se);ans$p[k]<-2*pnorm(-abs(ans$beta[k]/se))
}
write_out(ans,'ipw_stable_bootstrap_results.csv')
write_out(as.data.frame(betas),'ipw_bootstrap_coefficients.csv')
writeLines(c('Original education categories are adjusted through joint wave/education strata in CHARLS new sensitivity models.','This avoids infinite education coefficients seen when very rare education groups had no events.','Primary models were not refitted or changed.','Selection models and 1st/99th percentile weight limits were re-estimated in each of 200 participant bootstrap resamples.','Bootstrap normal-theory 95% CIs and P values include estimation of weights.'),file.path(out,'stable_model_notes.txt'))
cat('Stable CHARLS and IPW bootstrap analyses finished\n')
