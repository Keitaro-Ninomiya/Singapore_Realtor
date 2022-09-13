library(dplyr)
source("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Data_Collection/Local/SalesPerson/Scrape/Code.R")
dta=update()[[1]]
dtaPast=update()[[2]]
View(dta %>% arrange(desc(Last_Obs)))
View(dtaPast %>% arrange(desc(Exit_Date)))
write.csv(dta,"C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents.csv",
          row.names = FALSE)
write.csv(dtaPast,"C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past.csv",
          row.names = FALSE)
