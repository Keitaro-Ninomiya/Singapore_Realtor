library(dplyr)
library(tidyr)
library(lubridate)
library(hms)
library(lfe)
library(fixest)
library(modelsummary)
library(stargazer)

path_draft="C:/Users/Keitaro Ninomiya/Desktop/Singapore_Realtor_Draft"

dtaSales  = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaSales_v1.csv")
dtaAgents = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_v1.csv")
dtaAgents_Past = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past_v1.csv")

dtaSales= dtaSales %>%
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
         Time_from_register=transaction_date-registration_start_date)

Agencies = Agents %>% group_by(estate_agent_license_no,estate_agent_name) %>%
  count()

source("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Singapore_Realtor_Git/Results/DID/Regression/Code.R")

Size=0.75
Length=3
Group=6

Reg2(dtaAgents,Group,Size,Length)
results=Reg2(dtaAgents,Group,Size,Length,"latex")
fileConn<-file(paste(path_draft,"/Results/Sales/Results.txt",sep=""))
writeLines(results, fileConn)
close(fileConn)

results=Reg3(dtaAgents,Group,Length,"latex")
fileConn<-file(paste(path_draft,"/Results/Sales/Results_Hetro.txt",sep=""))
writeLines(results, fileConn)
close(fileConn)

text=Reg4(dtaAgents,Group,Size,"2W",Length,"latex")
fileConn<-file(paste(path_draft,"/Results/Sales/Results_Rep.txt",sep=""))
writeLines(text, fileConn)
close(fileConn)
