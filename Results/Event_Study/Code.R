library(dplyr)
library(lubridate)
library(hms)
library(lfe)
library(modelsummary)
library(ggplot2)

dtaSales  = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaSales_v2.csv")
dtaAgents = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_v1.csv")
dtaAgents_Past = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past_v1.csv")

dtaSales= dtaSales %>% distinct() %>%
  rename(registration_no=salesperson_reg_num)
dtaAgents = dtaAgents %>% mutate(Active=1) %>% select(!Last_Obs)
dtaAgents_Past = dtaAgents_Past %>% mutate(Active=0) %>% select(!Exit_Date) %>%
  distinct(salesperson_name,registration_no,estate_agent_name,estate_agent_license_no,Active,.keep_all=TRUE)
Agents=rbind(dtaAgents,dtaAgents_Past)

#Merge
dtaAgents = left_join(Agents,dtaSales,by=c("registration_no","salesperson_name")) %>%
  mutate(registration_start_date=ymd(registration_start_date),
         transaction_date=ymd(transaction_date))
dtaAgents =  dtaAgents  %>%
  mutate(CCR=case_when(district%in%c("9","10","11")~1,
                       TRUE~0)) %>%
  group_by(registration_no) %>%
  arrange(transaction_date) %>%
  mutate(index=row_number(transaction_date),
         final=max(index),
         CCR=case_when(district%in%c("9","10","11")~1,
                       TRUE~0),
         CCR_Total=sum(CCR,na.rm = T),
         CCR_Ratio=CCR_Total/final,
         tenure=ym("2022-07")-registration_start_date,
         Time_from_register=transaction_date-registration_start_date)

Agencies = Agents %>% group_by(estate_agent_license_no,estate_agent_name) %>%
  count()

Reg1=function(dta,month,Ratio,FE){
  CntInt=interval(ymd("2021-09-01")%m-%months(month),ymd("2021-9-01"))
  TrtInt=interval(ymd("2021-09-01"),ymd("2021-09-01")%m+%months(month))

  dta = dta %>%
    filter(registration_start_date%within%CntInt|registration_start_date%within%TrtInt) %>%
    filter(floor_date(transaction_date,"month")>=floor_date(registration_start_date,"month")) %>%
    mutate(Trt=case_when(registration_start_date%within%TrtInt~1,
                         TRUE~0),
           registration_start_date=floor_date(registration_start_date,"month"))

  if(FE=="TT"){
    results=felm(log(final)~as.factor(registration_start_date):Large+Large+tenure|floor_date(registration_start_date,"month"),data=dta)
  }else if (FE=="FE"){
    results=felm(log(final)~as.factor(registration_start_date):Large+as.factor(registration_start_date)+tenure|estate_agent_name,data=dta)
  }else if (FE=="2W"){
    results=felm(log(final)~as.factor(registration_start_date):Large+tenure|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)
  }else{
    results=felm(log(final)~as.factor(registration_start_date):Large+Trt+Large+tenure,data=dta)
  }
  return(results)
}

Reg2=function(dta,month,Size,FE,Length){
  # Identify big agencies ####
  Agencies = Agencies %>% mutate(Small=case_when(n<=quantile(Agencies$n,Size)~1,
                                                 TRUE~0))
  dta =  dta %>% left_join(Agencies,by=c("estate_agent_license_no","estate_agent_name")) %>%
    rename(Cumulative_final=n)

  dtaCount =  dta %>%
    filter(Time_from_register<=31*Length,Time_from_register>=0) %>%
    group_by(registration_no) %>%
    count()

  dtaCount = left_join(dta %>% distinct(registration_no),dtaCount,by="registration_no") %>%
    as_tibble() %>%
    select(registration_no,n)
  dtaCount[is.na(dtaCount)]=0

  dta = left_join(dta,dtaCount,by="registration_no")


  CntInt=interval(ymd("2021-09-01")%m-%months(month),ymd("2021-9-01"))
  TrtInt=interval(ymd("2021-09-01"),ymd("2021-09-01")%m+%months(month))
  dta = dta %>%
    filter(registration_start_date%within%CntInt|registration_start_date%within%TrtInt) %>%
    filter(!floor_date(registration_start_date,"month")%in%c(ymd("2021-07-01"),ymd("2021-08-01")))

  if(FE=="TT"){
    results=felm(n~as.factor(floor_date(registration_start_date,"bimonth"))*Small+Small|floor_date(registration_start_date,"month"),data=dta%>%distinct(registration_no,.keep_all = TRUE))
  }else if (FE=="FE"){
    results=felm(n~as.factor(floor_date(registration_start_date,"bimonth")):Small+as.factor(registration_start_date)|estate_agent_name,data=dta %>% distinct(registration_no,.keep_all = TRUE))
  }else if (FE=="2W"){
    results=felm(n~Small:as.factor(floor_date(registration_start_date,"bimonth"))|estate_agent_name+floor_date(registration_start_date,"month"),data=dta %>% distinct(registration_no,.keep_all = TRUE))
  }else{
    results=felm(n~as.factor(floor_date(registration_start_date,"bimonth")):Small+as.factor(floor_date(registration_start_date,"month"))+Small,data=dta %>% distinct(registration_no,.keep_all = TRUE))
  }
  return(results)
}

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

Size=0.75
Length=3
Group=6

path="C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results/Event_Study"
ggsave(filename="Event_Study.png",plot=Plot(Reg2(dtaAgents,Group,Size,"2W",Length)),path = path)
