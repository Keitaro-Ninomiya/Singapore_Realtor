library(dplyr)
update=function(){
  dtaSt=read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaSales_v2.csv")
  temp <- tempfile()
  url="https://storage.data.gov.sg/cea-salesperson-residential-transaction-record/cea-salesperson-residential-transaction-record.zip"
  download.file(url,temp,mode="wb")
  dta_new= read.csv(unz(temp,
                        "cea-salespersons-property-transaction-records-residential.csv"),
                 header = TRUE)
  unlink(temp)
  
  CovList = rbind(tibble(Month=substr(month.name,start=1,stop=3),Month_Trans=c(1:12)),
                  tibble(Month=toupper(substr(month.name,start=1,stop=3)),Month_Trans=c(1:12)))
  dta_new = dta_new %>% 
    mutate(Month=substr(transaction_date,start=1,stop=3),
           Year_Trans =substr(transaction_date,start=5,stop=10))%>% 
    left_join(.,CovList,by="Month") %>% 
    mutate(transaction_date=ym(paste(Year_Trans,Month_Trans,sep = "-"))) %>% 
    select(!c(Month,Month_Trans,Year_Trans))
  dtaSt = dtaSt %>% mutate(transaction_date=ymd(transaction_date))
  
  dta = anti_join(dta_new,dtaSt,by=c("salesperson_reg_num","transaction_date","represented","town","transaction_type"))
  dta = rbind(dta,dtaSt) %>% unique()
  print(paste0("Ran on",Sys.Date(),Sys.time()))
  write.csv(dta,"C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaSales_v2.csv",row.names=FALSE)
}