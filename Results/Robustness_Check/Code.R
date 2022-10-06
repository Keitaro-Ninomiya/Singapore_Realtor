setwd("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results")
path_local ="C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor"
path_remote="C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor_Git"
path_draft="C:/Users/Keitaro Ninomiya/Desktop/Singapore_Realtor_Draft"

dtaAgents = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_v1.csv")
dtaAgents_Past = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past_v1.csv")

#Merge ####
source(paste(path_remote,"/Results/Robustness_Check/Merge/Code.R",sep=""))
dta=Merge(dtaAgents,dtaAgents_Past)

#Regression
Size=0.75
Group=6

source(paste(path_remote,"/Results/Robustness_Check/Regression/Code.R",sep=""))
Reg(dta,dtaAgents,dtaAgents_Past,Group)
results=etable(Reg(dta,dtaAgents,dtaAgents_Past,Group), tex = TRUE)
fileConn<-file(paste(path_draft,"/Results/Entry/Results.tex",sep=""))
writeLines(results, fileConn)
close(fileConn)

#EventStudy
results=Event_Study(dta,dtaAgents,dtaAgents_Past,Group)[[1]]
results=etable(results,tex = TRUE)
fileConn<-file(paste(path_draft,"/Results/Entry/Event_Study.tex",sep=""))
writeLines(results, fileConn)
close(fileConn)

library(ggplot2)
Plot=Event_Study(dta,dtaAgents,dtaAgents_Past,Group)[[2]]
ggplot(data=Plot,
       aes(x=seq(ymd("2021-01-01"),ymd("2021-12-01"),by="month"),
           y=mean))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = ymd("2021-09-01"),color="red")
ggsave(filename = "Entry.png",path = paste(path_remote,"/Results",sep = ""),width=3,height=2)

Plot %>%
  ggplot(aes(x=seq(ymd("2021-01-01"),ymd("2021-12-01"),by="month"),
             y=mean))+
  geom_rect(aes(xmin=ymd("2021-01-01"),xmax=ymd("2021-08-01"),ymin=-Inf,ymax=Inf),
            fill="cyan",alpha=0.01)+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  ggtitle("Event Study: General form")+
  geom_errorbar(aes(ymin=mean-sd*1.96,ymax=mean+sd*1.96),width=0.2,
                position=position_dodge(0.05))+
  theme_bw()
ggsave(filename = "Event_Study_Entry.png",path = paste(path_remote,"/Results",sep = ""),width=3,height=2)
