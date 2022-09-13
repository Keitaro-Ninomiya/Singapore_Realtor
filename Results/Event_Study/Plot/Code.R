Plot=function(results){
  EventStudyPlot = tibble(
    mean=c(results$coefficients[1:5],0,results$coefficients[6:11]),
    sd = c(results$se[1:5],0,results$se[6:11]))
  
  Date=seq(ym("2020-09"),ym("2022-08"),by="2 months")
  plot=ggplot(data=EventStudyPlot,aes(x=Date,y=mean))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 0)+
    geom_errorbar(aes(ymin=mean-sd*1.96,ymax=mean+sd*1.96),width=0.2,
                  position=position_dodge(0.05))+
    theme_bw()+
    ggtitle("EventStudy,7.5 quantile")
  return(plot)
}