library(dplyr)
library(lubridate)
library(hms)
library(rqpd)

dtaSales  = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaSales.csv")[,-c(1,2,3)]
dtaAgents = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents.csv") 
dtaAgents_Past = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past.csv") 
dtaPrice  = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/Transactions/dtaPrice.csv")[,-c(1,2,3)]

dtaSales= dtaSales %>% distinct() %>% 
  rename(registration_no=salesperson_reg_num) 
dtaAgents = dtaAgents %>% mutate(Active=1) %>% select(!Last_Obs)
dtaAgents_Past = dtaAgents_Past %>% mutate(Active=0) %>% select(!Exit_Date) %>% 
  distinct(salesperson_name,registration_no,estate_agent_name,estate_agent_license_no,Active,.keep_all=TRUE)
Agents=rbind(dtaAgents,dtaAgents_Past)

# Add month number####
CovList = rbind(tibble(Month=substr(month.name,start=1,stop=3),Month_Trans=c(1:12)),
                tibble(Month=toupper(substr(month.name,start=1,stop=3)),Month_Trans=c(1:12)))
dtaSales = dtaSales %>% 
  mutate(Month=substr(transaction_date,start=1,stop=3),
         Year_Trans =substr(transaction_date,start=5,stop=10))
dtaSales = left_join(dtaSales,CovList,by="Month") %>% 
  mutate(transaction_date=ym(paste(Year_Trans,Month_Trans,sep="-")))
dtaAgents = left_join(Agents,dtaSales,by=c("registration_no","salesperson_name")) %>% 
  mutate(registration_start_date=ymd(registration_start_date),
         transaction_date=ym(paste(Year_Trans,Month_Trans,sep="/")))
dtaAgents =  dtaAgents  %>%
  mutate(CCR=case_when(district%in%c("9","10","11")~1,
                       TRUE~0)) %>% 
  group_by(registration_no) %>% 
  arrange(transaction_date) %>% 
  mutate(index=row_number(transaction_date),
         final=max(index),
         CCR_Total=sum(CCR,na.rm = T),
         CCR_Ratio=CCR_Total/final,
         tenure=ym("2022-04")-registration_start_date) 

Reg1=function(dta,month,Size,FE){
  # Identify big agencies ####
  Agencies = Agents %>% group_by(estate_agent_license_no,estate_agent_name) %>% 
    count() 
  Agencies = Agencies %>% mutate(Small=case_when(n<=quantile(Agencies$n,Size)~1,
                                                 TRUE~0))
  
  CntInt=interval(ymd("2021-09-01")%m-%months(month),ymd("2021-9-01"))
  TrtInt=interval(ymd("2021-09-01"),ymd("2021-09-01")%m+%months(month))
  
  dta = dta %>% 
    filter(registration_start_date%within%CntInt|registration_start_date%within%TrtInt) %>% 
    filter(floor_date(transaction_date,"month")>=floor_date(registration_start_date,"month")) %>% 
    mutate(Trt=case_when(registration_start_date%within%TrtInt~1,
                         TRUE~0)) %>% 
    left_join(.,Agencies,by=c("estate_agent_license_no","estate_agent_name"))
  if(FE=="TT"){
    results=rqpd(log(final)~Trt*Small+Small+tenure|floor_date(registration_start_date,"month"),panel(method="pfe"),
                 data=dta)
  }else if (FE=="FE"){
    results=rqpd(log(final)~Trt*Small+Trt+tenure|estate_agent_name,panel(method="pfe"),
                 data=dta)
  }else{
    results=felm(log(final)~Trt*Small+Trt+Small+tenure,panel(method="pfe"),data=dta)
  }
  return(results)
}
Size=0.8
table=Reg1(dtaAgents,4,Size,"FE") %>% summary.rqpd() %>% as_tibble()
table=results %>% summary.rqpd() 
table$coefficients[1:15,] %>% as_tibble(rownames = NA) %>% write.csv(.,"C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results/Median_Regression/table.csv")
