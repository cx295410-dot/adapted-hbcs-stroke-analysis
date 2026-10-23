args <- commandArgs(trailingOnly=TRUE)
root <- args[1]; out <- args[2]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
packages <- c('survival','ggplot2','dplyr','readr','sandwich','mice','jsonlite','patchwork')
cat('PACKAGES\n'); print(setNames(vapply(packages,requireNamespace,logical(1),quietly=TRUE),packages))
file_info <- list(); schemas <- list(); activity <- list()
for (cohort in c('hrs','charls')) {
  for (kind in c('person_wave_scored','transition_intervals_scored','multistate_analytic')) {
    name <- paste0(cohort,'_',kind,'.rds')
    d <- readRDS(file.path(root,'data/derived',name))
    file_info[[name]] <- data.frame(file=name,rows=nrow(d),columns=ncol(d))
    schemas[[name]] <- data.frame(file=name,variable=names(d),class=vapply(d,function(x)paste(class(x),collapse='/'),''))
    cat('\nFILE',name,nrow(d),ncol(d),'\n',paste(names(d),collapse=', '),'\n')
    if (kind=='person_wave_scored') {
      cat('Interview date class/range: ',paste(class(d$interview_date),collapse='/'),range(d$interview_date,na.rm=TRUE),'\n')
      print(head(sort(unique(na.omit(d$interview_date))),5))
    }
    if (kind=='transition_intervals_scored') {
      e <- d[!is.na(d$age)&d$age>=50&d$state==0,]
      cat('State0 age50 transition counts\n');print(table(e$transition))
      cat('Activity missing by wave\n')
      a <- do.call(rbind,lapply(split(e,e$wave),function(x)data.frame(cohort=cohort,wave=x$wave[1],intervals=nrow(x),observed=sum(!is.na(x$score_activity)),missing_pct=100*mean(is.na(x$score_activity)),complete=sum(x$hbcs_complete))))
      print(a); activity[[cohort]] <- a
      cat('Missingness counts in proposed selection covariates\n')
      vs <- intersect(c('age','sex','education','education_years','race','ethnicity','bmi','smoking_ever','smoking_current','alcohol_any','hypertension_ever','diabetes_ever','marital_status','sleep_restless','mood_symptom_proportion','analysis_weight'),names(e))
      print(vapply(e[vs],function(x)sum(is.na(x)),numeric(1)))
    }
  }
}
write.csv(do.call(rbind,file_info),file.path(out,'data_file_info.csv'),row.names=FALSE)
write.csv(do.call(rbind,schemas),file.path(out,'actual_rds_schema.csv'),row.names=FALSE)
write.csv(do.call(rbind,activity),file.path(out,'charls_activity_coverage_and_hrs.csv'),row.names=FALSE)
cat('\nSession\n');print(sessionInfo())
