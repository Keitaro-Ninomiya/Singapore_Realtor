library(httr)
library(dplyr)
library(hms)
library(lubridate)

update=function(){
  setwd("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information")
  
  dtaSt = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents.csv") %>% 
    distinct(registration_no,estate_agent_license_no,.keep_all = TRUE)
  dtaPast = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past.csv") %>% 
    mutate(registration_start_date=ymd(registration_start_date),
           registration_end_date=ymd(registration_end_date),
           Exit_Date=ymd(Exit_Date)) %>% 
    distinct(registration_no,estate_agent_license_no,.keep_all = TRUE)
  
  Date = dtaSt %>% 
    select(Last_Obs)
  
  url="https://data.gov.sg/dataset/8f357dd4-96f6-4957-8a68-41b28e5e3f2f/download"
  GET(url = url,
      write_disk("cea_salesperson-info.zip",overwrite = T),
      verbose()) -> res
  dta_new= read.csv(unz("cea_salesperson-info.zip", "cea-salesperson-information.csv"),
                    header = TRUE)
  unlink("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/cea_salesperson-info.zip")
  
  dta_exit=setdiff(dtaSt %>% select(!Last_Obs),
                   dta_new) %>% 
    mutate(registration_start_date=ymd(registration_start_date),
           registration_end_date=ymd(registration_end_date))
  dta_exit = setdiff(dta_exit,dtaPast %>% select(!Exit_Date))
  
  dta_new=setdiff(dta_new,
                  dtaSt %>% select(!Last_Obs))
  
  
  if (nrow(dta_new)!=0& nrow(dta_exit)!=0){
    print("Entry and exit occured")
    dta = rbind(dtaSt %>% select(!Last_Obs),
                dta_new)
    
    Date[(nrow(dtaSt)+1):nrow(dta),]=as.character(Sys.time())
    dta = cbind(dta,Date)
    
    dta_exit$Exit_Date=Sys.Date()
    dtaPast = rbind(dtaPast,dta_exit)
    
    print(paste0("Ran on",Sys.time()))
    return(list(dta,dtaPast))
  }else if (nrow(dta_new)!=0& nrow(dta_exit)==0){
    print("Only Entry occured")
    dta = rbind(dtaSt %>% select(!Last_Obs),
                dta_new)
    
    Date[(nrow(dtaSt)+1):nrow(dta),]=as.character(Sys.time())
    dta = cbind(dta,Date)
    
    print(paste0("Ran on",Sys.time()))
    return(list(dta,dtaPast))
  }else if(nrow(dta_new)==0& nrow(dta_exit)!=0){
    dta_exit$Exit_Date=Sys.Date()
    dtaPast = rbind(dtaPast,dta_exit)
    
    print("Only Exit occured")
    print(paste0("Ran on",Sys.time()))
    return(list(dtaSt,dtaPast))
  } else{
    print("No New Records")
    print(paste0("Ran on",Sys.time()))
    return(list(dtaSt,dtaPast))
  }
}

