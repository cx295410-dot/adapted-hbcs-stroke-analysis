suppressPackageStartupMessages({library(dplyr)})
args<-commandArgs(trailingOnly=TRUE);root<-args[1];out<-args[2]
rows<-list();edu<-list()
for(cohort in c('HRS','CHARLS')){
  stem<-tolower(cohort);id<-if(cohort=='HRS')'hhidpn' else 'ID'
  p<-readRDS(file.path(root,'data/derived',paste0(stem,'_person_wave_scored.rds')))
  t<-readRDS(file.path(root,'data/derived',paste0(stem,'_transition_intervals_scored.rds')))
  a<-readRDS(file.path(root,'data/derived',paste0(stem,'_multistate_analytic.rds')))
  alive<-p[p$interviewed_alive%in%TRUE,]
  unmatched<-anti_join(alive,t[,c(id,'wave')],by=c(id,'wave'))
  lastwave<-max(p$wave)
  e<-t[!is.na(t$age)&t$age>=50,]
  cov<-if(cohort=='HRS')c('age','sex','education_years','race','ethnicity') else c('age','sex','education')
  complete0<-a[a$state==0,];complete1<-a[a$state==1,]
  final0<-complete0[complete.cases(complete0[,cov]),]
  valid1<-complete1[complete1$transition%in%c('1_to_1','1_to_2_death'),]
  final1<-valid1[complete.cases(valid1[,cov]),]
  counts<-c(source_participants=n_distinct(p[[id]]),person_waves=nrow(p),alive_interviewed_person_waves=nrow(alive),not_interviewed_alive=nrow(p)-nrow(alive),without_valid_interval=nrow(unmatched),last_study_wave_starts=sum(unmatched$wave==lastwave),other_invalid_starts=sum(unmatched$wave!=lastwave),valid_intervals=nrow(t),age_below_50=sum(t$age<50,na.rm=TRUE),age_missing=sum(is.na(t$age)),age50_intervals=nrow(e),incomplete_score=sum(!e$hbcs_complete),complete_score=nrow(a),complete_state0=nrow(complete0),complete_state1=nrow(complete1),other_complete_transitions=sum(a$transition=='other'),final_state0=nrow(final0),state0_covariate_missing=nrow(complete0)-nrow(final0),valid_complete_state1=nrow(valid1),final_state1=nrow(final1),state1_covariate_missing=nrow(valid1)-nrow(final1),state1_deaths=sum(valid1$transition=='1_to_2_death'),final_state0_participants=n_distinct(final0[[id]]),final_state1_participants=n_distinct(final1[[id]]))
  rows[[cohort]]<-data.frame(cohort=cohort,measure=names(counts),n=unname(counts))
  if(cohort=='CHARLS'){
    b<-complete0%>%arrange(ID,wave)%>%group_by(ID)%>%slice(1)%>%ungroup()
    b$cat<-cut(b$adapted_hbcs_18,c(-Inf,10,12,14,Inf),labels=c('0-10','11-12','13-14','15-18'))
    b$education_display<-case_when(b$education<=3~'Less than elementary school',b$education==4~'Elementary school',b$education%in%5:7~'Secondary or vocational school',b$education>=8~'College or postgraduate')
    for(group in c('Overall',levels(b$cat)))for(level in c('Less than elementary school','Elementary school','Secondary or vocational school','College or postgraduate')){
      z<-if(group=='Overall')b else b[b$cat==group,]
      edu[[length(edu)+1]]<-data.frame(category=group,education=level,participants=sum(z$education_display==level),denominator=nrow(z),percent=100*mean(z$education_display==level))
    }
  }
}
write.csv(bind_rows(rows),file.path(out,'flow_counts_current.csv'),row.names=FALSE)
write.csv(bind_rows(edu),file.path(out,'charls_baseline_education_categories.csv'),row.names=FALSE)
print(bind_rows(rows))
