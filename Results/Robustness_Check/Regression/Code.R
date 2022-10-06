library(tidyr)
library(fixest)

Reg=function(dta,dtaAgents,dtaAgents_Past,Group){
  dtaAgents = dtaAgents %>% mutate(Active=1)
  dtaAgents_Past = dtaAgents %>% mutate(Active=0)
  dtaAgents= rbind(dtaAgents[-7],dtaAgents_Past[-7]) %>%
    distinct(registration_no,estate_agent_license_no,registration_start_date,.keep_all=TRUE) %>%
    mutate(registration_start_date=floor_date(ymd(registration_start_date),"months")) %>%
    mutate(estate_agent_license_no=case_when(estate_agent_license_no%in%Agency_List$estate_agent_license_no~estate_agent_license_no,
                                             TRUE~"Small"))
  Agency = dtaAgents%>%
    group_by(estate_agent_license_no) %>%
    count()
  AgencyCount = Agency %>% filter(estate_agent_license_no!="Small")
  Agency = Agency %>%
    mutate(Trt=case_when(n>=quantile(AgencyCount$n,Size) & estate_agent_license_no!="Small"~0,
                         TRUE~1))

  dta = dta %>%
    left_join(.,Agency,by=c("estate_agent_license_no")) %>%
    drop_na(Trt) %>%
    group_by(registration_start_date,estate_agent_license_no,Trt) %>%
    count() %>%
    mutate(Post=case_when(registration_start_date>=ymd("2021-09-01")~1,
                          TRUE~0))

  dtaOrg = dta %>% as_tibble() %>%
    select(registration_start_date,estate_agent_license_no) %>%
    expand(registration_start_date,estate_agent_license_no) %>%
    left_join(.,Agency,by=c("estate_agent_license_no")) %>%
    rename(Size=n)

  dta = dtaOrg %>% left_join(.,dta,by=c("registration_start_date","estate_agent_license_no","Trt")) %>%
    as_tibble() %>%
    mutate(Entry=ifelse(is.na(n)==T,0,n),
           Post=case_when(registration_start_date>=ymd("2021-09-01")~1,
                          TRUE~0))

  results=fepois(Entry~Trt:Post+Trt+Post|csw0(as.character(registration_start_date), estate_agent_license_no),
                 data=dta %>% filter(registration_start_date<=ymd("2021-09-01")%m+% months(Group)))
  return(results)
}

Event_Study=function(dta,dtaAgents,dtaAgents_Past,Group){
  dtaAgents = dtaAgents %>% mutate(Active=1)
  dtaAgents_Past = dtaAgents %>% mutate(Active=0)
  dtaAgents= rbind(dtaAgents[-7],dtaAgents_Past[-7]) %>%
    distinct(registration_no,estate_agent_license_no,registration_start_date,.keep_all=TRUE) %>%
    mutate(registration_start_date=floor_date(ymd(registration_start_date),"months"))
  Agency = dtaAgents %>%
    group_by(estate_agent_license_no,estate_agent_name) %>%
    count()
  Agency = Agency %>%
    mutate(Trt=case_when(n>=quantile(Agency$n,Size)~0,
                           TRUE~1))

  dta = dta %>%
    left_join(.,Agency,by=c("estate_agent_license_no","estate_agent_name")) %>%
    drop_na(Trt) %>%
    group_by(registration_start_date,estate_agent_license_no,Trt) %>%
    count() %>%
    mutate(Post=case_when(registration_start_date>=ymd("2021-09-01")~1,
                          TRUE~0))

  dtaOrg = dta %>% as_tibble() %>%
    select(registration_start_date,estate_agent_license_no) %>%
    expand(registration_start_date,estate_agent_license_no) %>%
    left_join(.,Agency,by=c("estate_agent_license_no")) %>%
    rename(Size=n)

  dta = dtaOrg %>% left_join(.,dta,by=c("registration_start_date","estate_agent_license_no","Trt")) %>%
    as_tibble() %>%
    mutate(Entry=ifelse(is.na(n)==T,0,n),
           Post=case_when(registration_start_date>=ymd("2021-09-01")~1,
                          TRUE~0))

  results=fepois(Entry~Trt:as.character(registration_start_date)|as.character(registration_start_date)+estate_agent_license_no,
                 data=dta %>% filter(registration_start_date<=ymd("2021-09-01")%m+% months(Group),
                                     !estate_agent_license_no%in%c("L3002382K","L3008022J","L3008899K","L3009250K")))

  EventStudyPlot = tibble(
    mean=results$coefficients[1:12],
    sd = results$se[1:12])

  return(list(results,EventStudyPlot))
}

