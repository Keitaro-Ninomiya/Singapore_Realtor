library(dplyr)
library(hms)
library(lubridate)
Merge=function(dtaAgents,dtaAgents_Past){
  dtaAgents = dtaAgents %>% mutate(Active=1)
  dtaAgents_Past = dtaAgents %>% mutate(Active=0)
  dtaAgents= rbind(dtaAgents[-7],dtaAgents_Past[-7]) %>%
    distinct(registration_no,estate_agent_license_no,registration_start_date,.keep_all=TRUE) %>%
    mutate(registration_start_date=floor_date(ymd(registration_start_date),"months"))
  Agency_List = dtaAgents %>% group_by(estate_agent_license_no,estate_agent_name) %>%
    count() %>% as_tibble() %>% select(estate_agent_license_no) %>% as.list()

  Dates=seq(ymd("2021/01/01"),ymd("2022/06/01"),by="month")
  dta = expand.grid(Agency_List[[1]],y=Dates, KEEP.OUT.ATTRS = FALSE) %>% as_tibble() %>%
    rename(estate_agent_license_no=1,
           registration_start_date=2) %>%
    left_join(.,dtaAgents,by=c("estate_agent_license_no","registration_start_date"))
  return(dta)
}
