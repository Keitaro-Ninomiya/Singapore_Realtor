library(dplyr)
library(lubridate)
library(hms)

dtaSt = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents.csv")
dtaSt1 = dtaSt %>% filter(is.na(ymd(registration_start_date))==F) %>% 
  mutate(registration_start_date=ymd(registration_start_date),
         registration_end_date=ymd(registration_end_date),
         Last_Obs=ymd_hms(Last_Obs))
dtaSt2 = dtaSt %>% filter(is.na(ymd(registration_start_date))==T) %>% 
  mutate(registration_start_date=dmy(registration_start_date),
         registration_end_date=dmy(registration_end_date),
         Last_Obs=dmy(Last_Obs))
dtaSt = rbind(dtaSt1,dtaSt2) %>% 
  distinct(.,salesperson_name,registration_no,registration_start_date,registration_end_date,estate_agent_name,estate_agent_license_no,.keep_all = TRUE) %>% 
  arrange(.,desc(Last_Obs))
write.csv(dtaSt,"C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents.csv",row.names = FALSE)

dtaSt2 = dtaSt %>% filter(nchar(Last_Obs)<=10) %>% 
  mutate(Last_Obs=dmy(Last_Obs),
         registration_start_date=dmy(registration_start_date),
         registration_end_date=dmy(registration_end_date))
dtaSt = rbind(dtaSt1,dtaSt2) %>% 
  arrange(Last_Obs) %>% 
  distinct(.,across(colnames(dtaSt)[-7]),.keep_all = TRUE)

AgentTrans = dtaSt %>%
  group_by(registration_no) %>% 
  count()

TS_Agent = dtaSt %>% 
  count(registration_start_date)
TS_AgentMon = TS_Agent %>% 
  mutate(start_month=format(as.Date(registration_start_date),"%Y-%m")) %>% 
  group_by(start_month) %>% 
  summarize(count=sum(n))
TS_Agency= dtaSt %>% 
  group_by(estate_agent_license_no,estate_agent_name) %>% 
  count(registration_start_date)
TS_AgencyMon = TS_Agency %>% 
  mutate(start_month=format(as.Date(registration_start_date),"%Y-%m")) %>% 
  group_by(start_month,estate_agent_license_no,estate_agent_name) %>% 
  summarize(count=sum(n))
TS_AgencyMonAgg = TS_Agency %>% 
  mutate(start_month=format(as.Date(registration_start_date),"%Y-%m")) %>% 
  group_by(start_month,estate_agent_license_no,estate_agent_name) %>% 
  summarise(count=sum(n)) %>% as_tibble() %>% 
  group_by(start_month) %>%
  summarise(mean=mean(count,na.rm=TRUE),
            sd=sd(count,na.rm=TRUE))


ggplot(data=TS_Agent,aes(x=registration_start_date,y=n,group=1))+
  geom_line()+
  ylim(0,2000)
ggplot(data=TS_AgentMon,aes(x=start_month,y=count,group=1))+
  geom_line()+
  ylim(0,2000)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
ggplot(data=TS_Agency,aes(x=registration_start_date,y=n,group=estate_agent_name,color=estate_agent_name))+
  geom_line()+
  theme(legend.position="none")+
  ylim(0,500)
ggplot(data=TS_AgencyMon,aes(x=start_month,y=count,group=estate_agent_name,color=estate_agent_name))+
  geom_line()+
  theme(legend.position="none")+
  ylim(0,1000)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(data=TS_AgencyMonAgg,aes(x=start_month,y=mean,group=1))+
  geom_line()+
  theme(legend.position="none")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(data=TS_AgencyMonAgg,aes(x=start_month,y=sd,group=1))+
  geom_line()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

