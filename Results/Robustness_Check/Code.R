setwd("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Results")
path_local ="C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor"
path_remote="C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Singapore_Realtor_Git"

dtaAgents = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_v1.csv")
dtaAgents_Past = read.csv("C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Singapore_Realtor/Raw_Data/CEA_Salesperson_Information/dtaAgents_Past_v1.csv")

#Merge ####
source(paste(path_remote,"/Results/Robustness_Check/Merge/Code.R",sep=""))
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

#Regression
Agency = dtaAgents %>%
  group_by(estate_agent_license_no,estate_agent_name) %>%
  count()
Agency = Agency %>%
  mutate(Large=case_when(n>=quantile(Agency$n,Size)~1,
                         TRUE~0))
dta = dta %>%
  left_join(.,Agency,by=c("estate_agent_license_no","estate_agent_name")) %>%
  drop_na(Large) %>%
  group_by(registration_start_date,estate_agent_license_no,Large) %>%
  count() %>%
  mutate(Post=case_when(registration_start_date>=ymd("2021-09-01")~1,
                        TRUE~0))

#Regression
Size=0.75
Length=3
Group=12

fepois(n~Large:Post+Large+Post|as.character(registration_start_date)+estate_agent_license_no,data=dta) %>% summary()
etable(fepois(n~Large:Post+Large+Post,data=dta) %>% summary(), tex = TRUE)

#EventStudy
fepois(n~Large:as.character(registration_start_date)|as.character(registration_start_date)+estate_agent_license_no,data=dta) %>% summary()
