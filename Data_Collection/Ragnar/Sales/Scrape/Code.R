library(dplyr)
update=function(){
  dtaSt=read.csv("/home/keitaro2/Singapore_Realtor/Raw_Data/CEA_Salespersons_Property_Transaction_Records/dtaSales.csv")
  temp <- tempfile()
  url="https://storage.data.gov.sg/cea-salesperson-residential-transaction-record/cea-salesperson-residential-transaction-record.zip"
  download.file(url,temp)
  dta_new= read.csv(unz(temp,"cea-salespersons-property-transaction-records-residential.csv"),
                 header = TRUE)
  unlink(temp)
  
  dta = anti_join(dtaSt,dta_new)
  print(paste0("Ran on",Sys.Date(),Sys.time()))
  write.csv(dta,"/home/keitaro2/Singapore_Realtor/Raw_Data/CEA_Salespersons_Property_Transaction_Records/dtaSales.csv")
}




