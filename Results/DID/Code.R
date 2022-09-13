library(dplyr)
library(tidyr)
library(lubridate)
library(hms)
library(lfe)
library(fixest)
library(modelsummary)
library(stargazer)

dtaSales  = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaSales.csv")
dtaAgents = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents.csv")
dtaAgents_Past = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past.csv")

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
         tenure=ym("2022-07")-registration_start_date,
         Time_from_register=transaction_date-registration_start_date)

Agencies = Agents %>% group_by(estate_agent_license_no,estate_agent_name) %>%
  count()

source("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Singapore_Realtor_Git/Results/DID/Functions.R")
Size=0.75
Length=3
Group=6

results=Reg2(dtaAgents,Group,Size,Length)
setwd("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results")
fileConn<-file("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results/DID/PoissonTable.txt")
writeLines(results, fileConn)
close(fileConn)

results=Reg3(dtaAgents,Group,Length)
setwd("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results")
fileConn<-file("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results/DID/PoissonTable_Hetro.txt")
writeLines(results, fileConn)
close(fileConn)

text=Reg4(dtaAgents,Group,Size,"2W",Length)
fileConn<-file("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results/DID/PoissonTable_Rep.txt")
writeLines(text, fileConn)
close(fileConn)

Size=0.75
Length=3
Group=12

results=Reg2(dtaAgents,Group,Size,Length)
setwd("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results")
fileConn<-file("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results/DID/PoissonTable_12.txt")
writeLines(results, fileConn)
close(fileConn)
