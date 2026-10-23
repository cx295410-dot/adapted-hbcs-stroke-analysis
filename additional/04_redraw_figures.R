suppressPackageStartupMessages({library(ggplot2);library(dplyr);library(patchwork);library(grid)})
args<-commandArgs(trailingOnly=TRUE);root<-args[1];out<-args[2];figdir<-file.path(out,'figures')
dir.create(figdir,recursive=TRUE,showWarnings=FALSE)
pal<-c(HRS='#355C7D',CHARLS='#B76045')
theme_set(theme_classic(base_size=12)+theme(plot.title=element_text(face='bold',size=12),axis.title=element_text(size=11),axis.text=element_text(size=10)))
save_both<-function(p,name,width,height){
  ggsave(file.path(figdir,paste0(name,'.png')),p,width=width,height=height,dpi=400,bg='white')
  ggsave(file.path(figdir,paste0(name,'.pdf')),p,width=width,height=height,bg='white')
}
flow<-read.csv(file.path(out,'flow_counts_current.csv'))
num<-function(c,m)flow$n[flow$cohort==c&flow$measure==m]
fmt<-function(x)format(x,big.mark=',',scientific=FALSE,trim=TRUE)
flow_panel<-function(cohort){
  n<-function(m)fmt(num(cohort,m));labels<-c(paste0('Source participants\nN = ',n('source_participants')),paste0('Constructed person-wave records\nN = ',n('person_waves')),paste0('Interviewed and alive\nN = ',n('alive_interviewed_person_waves'),' person-waves'),paste0('Valid adjacent-wave records\nN = ',n('valid_intervals'),' intervals'),paste0('Interval-start age at least 50\nN = ',n('age50_intervals'),' intervals'),paste0('Complete nine-domain score\nN = ',n('complete_score'),' intervals'),if(cohort=='HRS')paste0('Model 3 risk sets\nStroke-free: ',n('final_state0'),' intervals\nPoststroke: ',n('final_state1'),' intervals') else paste0('Replication risk set\nStroke-free: ',n('final_state0'),' intervals'))
  boxes<-data.frame(x=.26,y=seq(6.4,.4,length.out=7),label=labels,index=1:7)
  exlabels<-c(paste0('Not interviewed alive\nN = ',n('not_interviewed_alive'),' person-waves'),paste0('No valid adjacent interval\nLast study wave: ',n('last_study_wave_starts'),' starts\nOther unascertainable starts: ',n('other_invalid_starts')),paste0('Age below 50: ',n('age_below_50'),' intervals\nMissing age: ',n('age_missing'),' intervals'),paste0('One or more domains missing\nN = ',n('incomplete_score'),' intervals'),if(cohort=='HRS')paste0('Missing Model 3 covariates\nStroke-free: ',n('state0_covariate_missing'),' intervals\nPoststroke: ',n('state1_covariate_missing'),' intervals') else paste0('350 intervals starting after stroke\n311 valid intervals (20 deaths): not modeled\n39 reversing reports: outside modeled transitions'))
  ex<-data.frame(x=.79,y=c(4.9,3.9,2.9,1.9,.9),label=exlabels)
  p<-ggplot()+geom_rect(data=boxes,aes(xmin=.015,xmax=.50,ymin=y-.32,ymax=y+.32),fill='#F2F5F6',color='#AAB4BC',linewidth=.4)+geom_rect(data=boxes[7,],aes(xmin=.015,xmax=.50,ymin=y-.35,ymax=y+.35),fill=pal[cohort],color=pal[cohort],linewidth=.4)+geom_text(data=boxes[1:6,],aes(x,y,label=label),size=3.7,lineheight=1.15)+geom_text(data=boxes[7,],aes(x,y,label=label),size=3.6,lineheight=1.12,color='white',fontface='bold')+geom_segment(data=boxes[1:6,],aes(x=x,xend=x,y=y-.33,yend=y-.65),color='#86939B',linewidth=.4,arrow=arrow(length=unit(.055,'inches')))+geom_rect(data=ex,aes(xmin=.565,xmax=1.0,ymin=y-.30,ymax=y+.30),fill='#F6F6F6',color='#D2D7DB',linewidth=.3)+geom_segment(data=ex,aes(x=.26,xend=.55,y=y,yend=y),color='#86939B',linewidth=.4,arrow=arrow(length=unit(.055,'inches')))+geom_text(data=ex,aes(x,y,label=label),size=3.13,lineheight=1.13)+annotate('text',x=.02,y=7.02,label=cohort,hjust=0,fontface='bold',size=5,color=pal[cohort])+annotate('text',x=.52,y=6.40,label='Participants',hjust=0,size=3,color='#687681')+annotate('text',x=.52,y=5.66,label='Person-waves',hjust=0,size=3,color='#687681')+annotate('text',x=.52,y=3.58,label='Intervals',hjust=0,size=3,color='#687681')+coord_cartesian(xlim=c(0,1.01),ylim=c(-.02,7.3),clip='off')+theme_void()+theme(plot.margin=margin(6,10,6,5))
  p
}
save_both(flow_panel('HRS')+flow_panel('CHARLS')+plot_layout(ncol=2),'Figure_1_Study_flow',15,9)

rcs<-read.csv(file.path(root,'results/rcs_curve_predictions.csv'))
ord<-data.frame(cohort=c(rep('HRS',3),rep('CHARLS',2)),transition=c('0_to_1_stroke','0_to_2_death','1_to_2_death','0_to_1_stroke','0_to_2_death'))
lab<-c('0_to_1_stroke'='First recorded stroke','0_to_2_death'='Death without recorded stroke','1_to_2_death'='Poststroke mortality')
ptext<-function(p)if(p<.0001)'<0.0001' else if(p<.001)sprintf('= %.4f',p) else sprintf('= %.3f',p)
ps<-list()
for(i in seq_len(nrow(ord))){
  d<-rcs[rcs$cohort==ord$cohort[i]&rcs$transition==ord$transition[i],]
  stopifnot(all(d$lower_95>0))
  ps[[i]]<-ggplot(d,aes(score,hazard_ratio))+geom_ribbon(aes(ymin=lower_95,ymax=upper_95),fill=pal[ord$cohort[i]],alpha=.14)+geom_line(color=pal[ord$cohort[i]],linewidth=.9)+geom_hline(yintercept=1,color='#77848C',linetype='dashed',linewidth=.35)+geom_vline(xintercept=12,color='#9EA8AE',linetype='dotted',linewidth=.4)+scale_y_log10(breaks=c(.1,.2,.3,.5,1,2,3,5,10),labels=function(x)format(x,trim=TRUE))+labs(x='hBCS',y='Hazard ratio (95% CI)',title=paste0(LETTERS[i],'  ',ord$cohort[i],': ',lab[ord$transition[i]]),subtitle=paste0('P overall ',ptext(d$p_overall[1]),'\nP nonlinearity ',ptext(d$p_nonlinear[1])))+theme(plot.subtitle=element_text(size=10),plot.margin=margin(8,10,8,8))
}
save_both((ps[[1]]+ps[[2]]+ps[[3]])/(ps[[4]]+ps[[5]])+plot_layout(heights=c(1,1)),'Figure_3_RCS_dose_response',13,7.5)

comp<-read.csv(file.path(root,'results/targeted_audit_20260923_222008/component_summary_from_current_rds.csv'))
names_domain<-c('score_smoking'='Smoking','score_alcohol'='Alcohol','score_activity'='Physical activity','score_bmi'='BMI','score_hypertension'='Hypertension','score_diabetes'='Diabetes','score_sleep'='Sleep','score_mood'='Mood','score_social'='Social connection')
long<-bind_rows(lapply(0:2,function(k)data.frame(cohort=comp$cohort,domain=comp$component,score=factor(k,levels=0:2),percent=100*comp[[paste0('score_',k)]]/comp$observed)))
long$domain<-factor(long$domain,levels=rev(names(names_domain)),labels=rev(unname(names_domain)))
pcomp<-ggplot(long,aes(percent,domain,fill=score))+geom_col(width=.72,color='white',linewidth=.25,position='stack')+facet_wrap(~cohort,nrow=1)+scale_fill_manual(values=c('#AFBBC5','#DABE88','#4F8187'),labels=c('0','1','2'))+scale_x_continuous(limits=c(0,100),breaks=seq(0,100,25))+labs(x='Observed eligible intervals (%)',y=NULL,fill='Domain score')+theme(legend.position='bottom',strip.background=element_blank(),strip.text=element_text(face='bold'))
save_both(pcomp,'Figure_S2_Component_score_distribution',11.5,5)
comp$domain<-factor(comp$component,levels=rev(names(names_domain)),labels=rev(unname(names_domain)))
pmiss<-ggplot(comp,aes(missing_pct,domain,color=cohort))+geom_segment(aes(x=0,xend=missing_pct,yend=domain),color='#D9E0E4',linewidth=.6)+geom_point(size=2.4)+geom_text(aes(label=sprintf('%.1f%%',missing_pct)),hjust=-.2,size=3.3)+facet_wrap(~cohort,nrow=1)+scale_color_manual(values=pal)+scale_x_continuous(limits=c(0,70),breaks=seq(0,60,20))+labs(x='Eligible intervals with missing component (%)',y=NULL)+theme(legend.position='none',strip.background=element_blank(),strip.text=element_text(face='bold'))
save_both(pmiss,'Figure_S3_Component_missingness',11.5,5)

h<-read.csv(file.path(out,'hbcs_integer_distribution.csv'))
ph<-ggplot(h,aes(score,percent,fill=cohort))+geom_col(width=.9,color='white',linewidth=.3)+facet_wrap(~cohort,nrow=1)+scale_fill_manual(values=pal)+scale_x_continuous(limits=c(-.5,18.5),breaks=seq(0,18,3))+labs(x='hBCS at first complete-score stroke-free interval',y='Participants (%)')+theme(legend.position='none',strip.background=element_blank(),strip.text=element_text(face='bold'))
save_both(ph,'Figure_S5_hBCS_histogram',11.5,4.7)

cif<-read.csv(file.path(out,'cif_curve_data.csv'));nr<-read.csv(file.path(out,'cif_number_at_risk.csv'))
cif$cohort<-factor(cif$cohort,levels=c('HRS','CHARLS'))
nr$cohort<-factor(nr$cohort,levels=c('HRS','CHARLS'))
cols<-c('0-10'='#A34E43','11-12'='#BE9A55','13-14'='#658B87','15-18'='#355C7D')
stepcif<-cif%>%group_by(cohort,category,endpoint)%>%group_modify(function(d,key){
  d<-d[order(d$time),];before<-d[-nrow(d),];before$time<-d$time[-1];bind_rows(d,before)%>%arrange(time)
})%>%ungroup()
# For each event time the preceding value must be drawn before the updated value.
stepcif<-cif%>%group_by(cohort,category,endpoint)%>%group_modify(function(d,key){
  d<-d[order(d$time),];parts<-list(d[1,]);if(nrow(d)>1)for(i in 2:nrow(d)){b<-d[i-1,];b$time<-d$time[i];parts[[length(parts)+1]]<-b;parts[[length(parts)+1]]<-d[i,]};bind_rows(parts)
})%>%ungroup()
stepcif$endpoint<-factor(stepcif$endpoint,levels=c('stroke','death'),labels=c('First recorded\nstroke','Death without\nrecorded stroke'))
pc<-ggplot(stepcif,aes(time,cif,color=category,fill=category))+geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.07,color=NA)+geom_path(linewidth=.7)+facet_grid(endpoint~cohort,scales='free_x')+scale_color_manual(values=cols)+scale_fill_manual(values=cols)+scale_y_continuous(labels=function(x)paste0(round(x*100),'%'))+labs(x='Years since entry',y='Unadjusted cumulative incidence',color='Entry hBCS',fill='Entry hBCS')+theme(legend.position='bottom',strip.background=element_blank(),strip.text=element_text(face='bold'),plot.margin=margin(8,8,0,8))
nr$category<-factor(nr$category,levels=rev(names(cols)))
pr<-ggplot(nr,aes(time,category,label=format(at_risk,big.mark=',',trim=TRUE),color=category))+geom_text(size=3.4)+facet_wrap(~cohort,nrow=1,scales='free_x')+scale_color_manual(values=cols,guide='none')+scale_x_continuous(expand=expansion(mult=c(.04,.06)))+labs(x='Years since entry',y='Entry hBCS',title='Number at risk immediately before each time')+theme_minimal(base_size=11)+theme(panel.grid=element_blank(),strip.background=element_blank(),strip.text=element_blank(),plot.title=element_text(size=10),axis.title.x=element_blank(),axis.text.y=element_text(size=10),plot.margin=margin(0,8,5,8))
save_both(pc/pr+plot_layout(heights=c(4,1)),'Figure_S4_Cumulative_incidence',13,7.5)
cat('Figures redrawn in ',figdir,'\n')
