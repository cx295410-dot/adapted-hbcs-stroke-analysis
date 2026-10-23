#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(survival);library(dplyr);library(ggplot2);library(splines)})
args <- commandArgs(trailingOnly=TRUE)
root <- normalizePath(args[1],mustWork=TRUE); out <- args[2]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
if(file.exists(file.path(out,'additional_model_results.csv')))stop('Results already exist. Use a new output directory.')
sink(file.path(out,'additional_analysis.log'),split=TRUE)
on.exit(sink(),add=TRUE)
set.seed(20261011)
dir.create(file.path(out,'figures'),showWarnings=FALSE)
write_result <- function(d,name)write.csv(d,file.path(out,name),row.names=FALSE,na='')
covs <- list(HRS=c('age','factor(sex)','education_years','factor(race)','factor(ethnicity)'),CHARLS=c('age','factor(sex)','factor(education)'))
cov_names <- list(HRS=c('age','sex','education_years','race','ethnicity'),CHARLS=c('age','sex','education'))
load_cohort <- function(cohort){
  stem <- tolower(cohort); id <- if(cohort=='HRS')'hhidpn' else 'ID'
  p <- readRDS(file.path(root,'data/derived',paste0(stem,'_person_wave_scored.rds')))
  t <- readRDS(file.path(root,'data/derived',paste0(stem,'_transition_intervals_scored.rds')))
  a <- readRDS(file.path(root,'data/derived',paste0(stem,'_multistate_analytic.rds')))
  for(k in c('p','t','a')){d <- get(k);d$id_cluster <- as.character(d[[id]]);assign(k,d)}
  stopifnot(!anyDuplicated(p[c('id_cluster','wave')]),!anyDuplicated(t[c('id_cluster','wave')]))
  list(p=p,t=t,a=a)
}
D <- list(HRS=load_cohort('HRS'),CHARLS=load_cohort('CHARLS'))
transition_names <- c('0_to_1_stroke','0_to_2_death','1_to_2_death')
model_results <- list(); rate_rows <- list()
fit_model <- function(d,cohort,trans,analysis,exposure='adapted_hbcs_18',extra=character(),weight=NULL){
  state0 <- if(trans=='1_to_2_death')1 else 0
  required <- c(exposure,cov_names[[cohort]],'interval_years',extra)
  d <- d[d$state==state0 & complete.cases(d[,required,drop=FALSE]) & d$interval_years>0,]
  d$hbcs_per3 <- d[[exposure]]/3;d$event <- as.integer(d$transition==trans)
  if(sum(d$event)<30)stop(paste('Too few events:',cohort,analysis,trans,sum(d$event)))
  rhs <- c('hbcs_per3',covs[[cohort]],extra,'strata(wave)','cluster(id_cluster)')
  form <- as.formula(paste('Surv(rep(0,nrow(d)),interval_years,event)~',paste(rhs,collapse='+')))
  fit <- if(is.null(weight))coxph(form,data=d,ties='efron',na.action=na.fail) else coxph(form,data=d,weights=d[[weight]],ties='efron',na.action=na.fail)
  if(anyNA(coef(fit)))stop(paste('Aliased coefficient',cohort,analysis,trans))
  ci <- summary(fit)$conf.int['hbcs_per3',];co <- summary(fit)$coefficients['hbcs_per3',]
  row <- data.frame(cohort=cohort,transition=trans,analysis=analysis,participants=n_distinct(d$id_cluster),intervals=nrow(d),events=sum(d$event),HR=ci['exp(coef)'],lower95=ci['lower .95'],upper95=ci['upper .95'],p=co['Pr(>|z|)'],beta=coef(fit)['hbcs_per3'],robust_se=co['robust se'])
  rownames(row)<-NULL
  list(row=row,fit=fit,data=d)
}

# Same primary model risk sets; only descriptive interval-based person-years added.
primary <- read.csv(file.path(root,'results/primary_transition_model_summary.csv'))
for(cohort in names(D)){
  a <- D[[cohort]]$a
  trs <- if(cohort=='HRS')transition_names else transition_names[1:2]
  for(trans in trs)for(model in paste0('Model_',1:3)){
    req <- switch(model,Model_1='adapted_hbcs_18',Model_2=c('adapted_hbcs_18','age','sex'),Model_3=c('adapted_hbcs_18',cov_names[[cohort]]))
    d <- a[a$state==ifelse(trans=='1_to_2_death',1,0)&complete.cases(a[,req,drop=FALSE]),]
    events <- sum(d$transition==trans);py <- sum(d$interval_years)
    r <- primary[primary$cohort==cohort&primary$transition==trans&primary$model==model,]
    stopifnot(nrow(r)==1,nrow(d)==r$model_intervals,events==r$model_events)
    rate_rows[[length(rate_rows)+1]] <- data.frame(cohort=cohort,transition=trans,model=model,participants=n_distinct(d$id_cluster),intervals=nrow(d),events=events,person_years_nominal=py,rate_per_1000=events/py*1000,HR=r$hazard_ratio,lower95=r$lower_95,upper95=r$upper_95)
  }
}
write_result(bind_rows(rate_rows),'Table2_person_years_and_rates.csv')

# Exact previous-wave matching, not lag on a filtered complete-case table.
for(cohort in names(D)){
  t <- D[[cohort]]$t;p <- D[[cohort]]$p
  lag <- p[,c('id_cluster','wave','adapted_hbcs_18')]
  names(lag)[3]<-'lagged_hbcs';lag$wave<-lag$wave+1L
  d <- left_join(t,lag,by=c('id_cluster','wave'))
  d <- d[!is.na(d$age)&d$age>=50,]
  trs <- if(cohort=='HRS')transition_names else transition_names[1:2]
  for(trans in trs){r<-fit_model(d,cohort,trans,'Previous-wave score',exposure='lagged_hbcs');model_results[[length(model_results)+1]]<-r$row}
}
cat('Lagged models completed\n')

# Duration among survivors with an observed incident stroke within study waves.
hrs <- D$HRS
inc <- hrs$t[hrs$t$transition=='0_to_1_stroke',c('id_cluster','wave','year','next_wave','next_year')]
inc <- inc[order(inc$id_cluster,inc$next_wave),];inc<-inc[!duplicated(inc$id_cluster),]
names(inc)[2:5]<-c('first_stroke_interval_wave','last_stroke_free_year','first_recorded_stroke_wave','first_recorded_stroke_year')
post <- left_join(hrs$a[hrs$a$state==1,],inc,by='id_cluster')
post <- post[!is.na(post$first_recorded_stroke_year)&post$wave>=post$first_recorded_stroke_wave,]
post$duration_recorded <- post$year-post$first_recorded_stroke_year
post$duration_midpoint <- post$year-(post$last_stroke_free_year+post$first_recorded_stroke_year)/2
for(spec in c('Same incident-stroke survivor sample','Adjusted for time since first recorded stroke','Adjusted for midpoint-approximated stroke duration')){
  d<-post;extra<-character()
  if(grepl('first recorded',spec)){d$duration<-d$duration_recorded;extra<-'duration'}
  if(grepl('midpoint',spec)){d$duration<-d$duration_midpoint;extra<-'duration'}
  r<-fit_model(d,'HRS','1_to_2_death',spec,extra=extra)
  model_results[[length(model_results)+1]]<-r$row
}
write_result(data.frame(measure=c('Incident survivor participants','Poststroke complete-score intervals','Minimum recorded duration years','Maximum recorded duration years'),n=c(n_distinct(post$id_cluster),nrow(post),min(post$duration_recorded),max(post$duration_recorded))),'poststroke_duration_sample.csv')
for(trans in transition_names[1:2]){
  r<-fit_model(D$CHARLS$a[D$CHARLS$a$wave==3&D$CHARLS$a$next_wave==4,],'CHARLS',trans,'CHARLS 2015-2018 only')
  model_results[[length(model_results)+1]]<-r$row
}
write_result(bind_rows(model_results),'additional_model_results.csv')

# Participant-level included/excluded comparison at the first eligible state0 interval.
compare_rows <- list(); categorical_rows <- list(); hist_rows <- list()
for(cohort in names(D)){
  e <- D[[cohort]]$t %>% filter(!is.na(age),age>=50,state==0) %>% arrange(id_cluster,wave) %>% group_by(id_cluster) %>% slice(1) %>% ungroup()
  e$included <- complete.cases(e[,c('adapted_hbcs_18',cov_names[[cohort]]),drop=FALSE])
  e$female <- ifelse(is.na(e$sex),NA,e$sex==2)
  e$partner <- ifelse(is.na(e$marital_status),NA,e$marital_status%in%c(1,2,3))
  vars <- c('age','female','bmi','smoking_current','hypertension_ever','diabetes_ever','partner')
  if(cohort=='HRS')vars<-c(vars,'education_years')
  for(v in vars){
    x<-e[[v]][e$included];y<-e[[v]][!e$included];nx<-sum(!is.na(x));ny<-sum(!is.na(y));binary<-!v%in%c('age','bmi','education_years')
    if(binary){px<-mean(x==1,na.rm=TRUE);py<-mean(y==1,na.rm=TRUE);sx<-sprintf('%d/%d (%.1f%%)',sum(x==1,na.rm=TRUE),nx,100*px);sy<-sprintf('%d/%d (%.1f%%)',sum(y==1,na.rm=TRUE),ny,100*py);den<-sqrt((px*(1-px)+py*(1-py))/2)} else {px<-mean(x,na.rm=TRUE);py<-mean(y,na.rm=TRUE);sx<-sprintf('%.2f (%.2f); n=%d',px,sd(x,na.rm=TRUE),nx);sy<-sprintf('%.2f (%.2f); n=%d',py,sd(y,na.rm=TRUE),ny);den<-sqrt((var(x,na.rm=TRUE)+var(y,na.rm=TRUE))/2)}
    compare_rows[[length(compare_rows)+1]]<-data.frame(cohort=cohort,characteristic=v,included_participants=sum(e$included),excluded_participants=sum(!e$included),included=sx,excluded=sy,absolute_SMD=ifelse(den>0,abs(px-py)/den,NA))
  }
  if(cohort=='CHARLS')for(level in sort(unique(e$education))){
    x<-e$education[e$included];y<-e$education[!e$included];px<-mean(x==level);py<-mean(y==level);den<-sqrt((px*(1-px)+py*(1-py))/2)
    compare_rows[[length(compare_rows)+1]]<-data.frame(cohort=cohort,characteristic=paste('Education category',level),included_participants=sum(e$included),excluded_participants=sum(!e$included),included=sprintf('%d/%d (%.1f%%)',sum(x==level),length(x),100*px),excluded=sprintf('%d/%d (%.1f%%)',sum(y==level),length(y),100*py),absolute_SMD=ifelse(den>0,abs(px-py)/den,NA))
  }
  base <- D[[cohort]]$a %>% filter(state==0) %>% arrange(id_cluster,wave) %>% group_by(id_cluster) %>% slice(1) %>% ungroup()
  expected<-if(cohort=='HRS')25785 else 7897;stopifnot(nrow(base)==expected)
  hist_rows[[cohort]]<-data.frame(cohort=cohort,score=base$adapted_hbcs_18)
}
write_result(bind_rows(compare_rows),'included_excluded_participants.csv')
hist<-bind_rows(hist_rows)
write_result(hist%>%count(cohort,score,name='participants')%>%group_by(cohort)%>%mutate(percent=100*participants/sum(participants))%>%ungroup(),'hbcs_integer_distribution.csv')
p_hist<-ggplot(hist,aes(score,fill=cohort))+geom_histogram(binwidth=1,boundary=-.5,color='white',linewidth=.25)+facet_wrap(~cohort,scales='free_y')+scale_x_continuous(breaks=seq(0,18,3),limits=c(-.5,18.5))+scale_fill_manual(values=c(HRS='#355C7D',CHARLS='#B76045'))+labs(x='hBCS at first complete-score stroke-free interval',y='Participants')+theme_classic(base_size=11)+theme(legend.position='none',strip.background=element_blank(),strip.text=element_text(face='bold'))
ggsave(file.path(out,'figures/Figure_S5_hBCS_histogram.png'),p_hist,width=9,height=4,dpi=300)
ggsave(file.path(out,'figures/Figure_S5_hBCS_histogram.pdf'),p_hist,width=9,height=4)

# Fixed entry score CIF. Follow contiguous valid state0 intervals until the first event or gap.
cif_rows <- list(); risk_rows <- list(); risk_tables <- list(); landmark_summaries<-list()
category <- function(x)factor(cut(x,breaks=c(-Inf,10,12,14,Inf),labels=c('0-10','11-12','13-14','15-18')),levels=c('0-10','11-12','13-14','15-18'))
for(cohort in names(D)){
  all <- D[[cohort]]$t
  bas <- D[[cohort]]$a %>% filter(state==0) %>% arrange(id_cluster,wave) %>% group_by(id_cluster) %>% slice(1) %>% ungroup()
  chunks<-split(all,all$id_cluster);land<-vector('list',nrow(bas))
  for(i in seq_len(nrow(bas))){
    b<-bas[i,];tr<-chunks[[b$id_cluster]];tr<-tr[order(tr$wave),];w<-b$wave;endyear<-b$year;status<-0L
    repeat{
      hit<-which(tr$wave==w&tr$state==0)
      if(length(hit)!=1)break
      row<-tr[hit,];endyear<-row$next_year
      if(row$transition=='0_to_1_stroke'){status<-1L;break}
      if(row$transition=='0_to_2_death'){status<-2L;break}
      if(row$transition!='0_to_0')break
      w<-row$next_wave
    }
    land[[i]]<-data.frame(id_cluster=b$id_cluster,entry_wave=b$wave,entry_year=b$year,score=b$adapted_hbcs_18,time=endyear-b$year,status=status)
  }
  l<-bind_rows(land);stopifnot(all(l$time>0));l$category<-category(l$score)
  l$status<-factor(l$status,levels=0:2,labels=c('censored','stroke','death'))
  write_result(l%>%group_by(category)%>%summarise(participants=n(),strokes=sum(status=='stroke'),deaths_without_recorded_stroke=sum(status=='death'),censored=sum(status=='censored'),maximum_followup=max(time),.groups='drop')%>%mutate(cohort=cohort),paste0(tolower(cohort),'_landmark_counts.csv'))
  landmark_summaries[[cohort]]<-data.frame(cohort=cohort,participants=nrow(l),strokes=sum(l$status=='stroke'),deaths=sum(l$status=='death'),maximum_followup=max(l$time))
  max_h<-if(cohort=='HRS')10 else 7
  for(cat in levels(l$category)){
    sub<-l[l$category==cat,];sf<-survfit(Surv(time,status)~1,data=sub)
    for(endpoint in c('stroke','death')){
      j<-match(endpoint,sf$states);stopifnot(!is.na(j))
      curve<-data.frame(cohort=cohort,category=cat,endpoint=endpoint,time=c(0,sf$time),cif=c(0,sf$pstate[,j]),lower=c(0,sf$lower[,j]),upper=c(0,sf$upper[,j]))
      cif_rows[[length(cif_rows)+1]]<-curve[curve$time<=max_h,]
      for(h in if(cohort=='HRS')c(5,10) else 5){
        if(max(sub$time)<h)next
        sm<-summary(sf,times=h,extend=FALSE)
        nr<-sum(sub$time>=h)
        risk_rows[[length(risk_rows)+1]]<-data.frame(cohort=cohort,category=cat,endpoint=endpoint,years=h,participants=nrow(sub),at_risk_just_before_horizon=nr,risk=sm$pstate[1,j],lower=sm$lower[1,j],upper=sm$upper[1,j],se=sm$std.err[1,j])
      }
    }
    for(time in if(cohort=='HRS')seq(0,10,2) else c(0,2,4,5,7))risk_tables[[length(risk_tables)+1]]<-data.frame(cohort=cohort,category=cat,time=time,at_risk=sum(sub$time>=time))
  }
}
cifs<-bind_rows(cif_rows);risks<-bind_rows(risk_rows);rts<-bind_rows(risk_tables)
diffs<-risks%>%group_by(cohort,endpoint,years)%>%mutate(reference_risk=risk[category=='0-10'],risk_difference=risk-reference_risk,rd_se=sqrt(se^2+se[category=='0-10']^2),rd_lower=risk_difference-1.96*rd_se,rd_upper=risk_difference+1.96*rd_se)%>%ungroup()
diffs[diffs$category=='0-10',c('risk_difference','rd_lower','rd_upper')]<-0
write_result(cifs,'cif_curve_data.csv');write_result(risks,'cif_landmark_risks.csv');write_result(diffs,'cif_landmark_risk_differences.csv');write_result(rts,'cif_number_at_risk.csv');write_result(bind_rows(landmark_summaries),'cif_landmark_sample.csv')
cifs$endpoint<-factor(cifs$endpoint,levels=c('stroke','death'),labels=c('First recorded stroke','Death without previously recorded stroke'))
cifs$category<-factor(cifs$category,levels=c('0-10','11-12','13-14','15-18'))
p_cif<-ggplot(cifs,aes(time,cif,color=category,fill=category))+geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.07,color=NA)+geom_step(linewidth=.7)+facet_grid(endpoint~cohort,scales='free_x')+scale_color_manual(values=c('#A34E43','#BE9A55','#658B87','#355C7D'))+scale_fill_manual(values=c('#A34E43','#BE9A55','#658B87','#355C7D'))+scale_y_continuous(labels=function(x)paste0(round(100*x),'%'))+labs(x='Years since first complete-score stroke-free interval',y='Unadjusted cumulative incidence',color='Entry hBCS',fill='Entry hBCS')+theme_classic(base_size=10)+theme(legend.position='bottom',strip.background=element_blank(),strip.text=element_text(face='bold'))
ggsave(file.path(out,'figures/Figure_S4_Cumulative_incidence.png'),p_cif,width=10,height=7,dpi=300)
ggsave(file.path(out,'figures/Figure_S4_Cumulative_incidence.pdf'),p_cif,width=10,height=7)

# Selection weighting focuses on activity in the population with the other eight domains complete.
charls<-D$CHARLS$t
scores<-paste0('score_',c('smoking','alcohol','activity','bmi','hypertension','diabetes','sleep','mood','social'))
others<-setdiff(scores,'score_activity')
target<-charls[!is.na(charls$age)&charls$age>=50&charls$state==0&complete.cases(charls[,c(others,cov_names$CHARLS),drop=FALSE]),]
target$observed<-as.integer(!is.na(target$score_activity))
target$event_type<-factor(target$transition)
target$partner<-as.integer(target$marital_status%in%c(1,2,3))
selection_form<-observed~ns(age,3)+factor(sex)+factor(education)+factor(wave)+ns(bmi,3)+factor(score_smoking)+factor(score_alcohol)+factor(score_hypertension)+factor(score_diabetes)+factor(score_sleep)+factor(score_mood)+partner+event_type
weight_data<-function(d){
  m<-glm(selection_form,data=d,family=binomial())
  if(!m$converged||anyNA(coef(m)))stop('Selection model did not converge or has aliases')
  d$p_observed<-predict(m,type='response')
  if(any(d$p_observed<=0|d$p_observed>=1))stop('Selection model positivity failure')
  prev<-mean(d$observed)
  d<-d[d$observed==1,];d$ipw_untruncated<-prev/d$p_observed
  qs<-quantile(d$ipw_untruncated,c(.01,.99))
  d$ipw_truncated<-pmax(qs[1],pmin(qs[2],d$ipw_untruncated))
  list(data=d,model=m,limits=qs)
}
wd<-weight_data(target);w<-wd$data
weight_summary<-data.frame(target_intervals=nrow(target),target_participants=n_distinct(target$id_cluster),observed_intervals=nrow(w),observed_participants=n_distinct(w$id_cluster),activity_missing_pct=100*mean(target$observed==0),predicted_probability_min=min(predict(wd$model,type='response')),predicted_probability_max=max(predict(wd$model,type='response')),weight_q01=wd$limits[1],weight_q99=wd$limits[2],weight_max_untruncated=max(w$ipw_untruncated),ESS_truncated=sum(w$ipw_truncated)^2/sum(w$ipw_truncated^2))
write_result(weight_summary,'ipw_diagnostics.csv')
model_results_ipw<-list()
for(trans in transition_names[1:2])for(spec in c('Untruncated activity-selection IPW','Activity-selection IPW truncated at 1st and 99th percentiles')){
  col<-if(startsWith(spec,'Untruncated'))'ipw_untruncated' else 'ipw_truncated'
  r<-fit_model(w,'CHARLS',trans,spec,weight=col)
  model_results_ipw[[length(model_results_ipw)+1]]<-r$row
}
ipw_result<-bind_rows(model_results_ipw)
balance_names<-c('age','bmi','sex','education','wave','score_smoking','score_alcohol','score_hypertension','score_diabetes','score_sleep','score_mood','partner')
balance<-list()
for(v in balance_names){
  x<-target[[v]];s<-w[[v]];continuous<-v%in%c('age','bmi')
  if(continuous){scale<-sd(x);balance[[length(balance)+1]]<-data.frame(variable=v,level='continuous',target=mean(x),observed=mean(s),weighted=weighted.mean(s,w$ipw_truncated),SMD_before=abs(mean(s)-mean(x))/scale,SMD_after=abs(weighted.mean(s,w$ipw_truncated)-mean(x))/scale)} else for(lev in sort(unique(x))){
    pt<-mean(x==lev);po<-mean(s==lev);pw<-weighted.mean(s==lev,w$ipw_truncated);scale<-sqrt(pt*(1-pt));balance[[length(balance)+1]]<-data.frame(variable=v,level=as.character(lev),target=pt,observed=po,weighted=pw,SMD_before=ifelse(scale>0,abs(po-pt)/scale,0),SMD_after=ifelse(scale>0,abs(pw-pt)/scale,0))
  }
}
write_result(bind_rows(balance),'ipw_balance.csv')
write_result(ipw_result,'ipw_model_results.csv')
saveRDS(target,file.path(out,'ipw_target_for_bootstrap.rds'))
writeLines(deparse(selection_form),file.path(out,'ipw_selection_formula.txt'))
writeLines(capture.output(sessionInfo()),file.path(out,'sessionInfo.txt'))
writeLines(c('All original input files were read only.','Primary score definitions retained at user request. CHARLS alcohol coding remains unresolved.','IPW target is restricted to complete other-eight-domain and Model 3 records.','Nominal person-years retain original interval-end event assignments.','CIF is unadjusted fixed-entry-score first-event follow-up, censored at the first gap.','CIF endpoint times use nominal survey years; CHARLS 10-year estimates were not produced.','No MI performed; activity selection weighting is the chosen missing-data sensitivity analysis.','IPW bootstrap is run separately to account for estimated weights.'),file.path(out,'analysis_notes.txt'))
cat('Additional analyses finished. Output: ',out,'\n')
