library(dplyr)
library(lubridate)
library(hms)

Merge=function(dtaSales,dtaAgents,dtaAgents_Past){
  dtaSales= dtaSales %>% 
    rename(registration_no=salesperson_reg_num) 
  dtaAgents = dtaAgents %>% mutate(Active=1) %>% select(!Last_Obs)
  dtaAgents_Past = dtaAgents_Past %>% mutate(Active=0) %>% select(!Exit_Date) %>% 
    distinct(salesperson_name,registration_no,estate_agent_name,estate_agent_license_no,Active,.keep_all=TRUE)
  Agents=rbind(dtaAgents,dtaAgents_Past) %>% 
    distinct(salesperson_name,registration_no,estate_agent_name,estate_agent_license_no,Active,.keep_all=TRUE)
  
  
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

  #tenure=ym("2022-07")-registration_start_date
  Agencies = Agents %>% group_by(estate_agent_license_no,estate_agent_name) %>% 
    count() 
  return(list(dtaAgents,Agencies))
}
