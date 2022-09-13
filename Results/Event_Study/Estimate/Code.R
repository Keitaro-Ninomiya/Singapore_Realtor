Reg1=function(dta,month,FE){
  CntInt=interval(ymd("2021-09-01")%m-%months(month),ymd("2021-9-01"))
  TrtInt=interval(ymd("2021-09-01"),ymd("2021-09-01")%m+%months(month))

  dta = dta %>%
    filter(registration_start_date%within%CntInt|registration_start_date%within%TrtInt) %>%
    filter(floor_date(transaction_date,"month")>=floor_date(registration_start_date,"month")) %>%
    mutate(Trt=case_when(registration_start_date%within%TrtInt~1,
                         TRUE~0),
           registration_start_date=floor_date(registration_start_date,"month"))

  if(FE=="TT"){
    results=fepois(log(final)~as.factor(registration_start_date):Large+Large+tenure|floor_date(registration_start_date,"month"),data=dta)
  }else if (FE=="FE"){
    results=fepois(log(final)~as.factor(registration_start_date):Large+as.factor(registration_start_date)+tenure|estate_agent_name,data=dta)
  }else if (FE=="2W"){
    results=fepois(log(final)~as.factor(registration_start_date):Large+tenure|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)
  }else{
    results=felm(log(final)~as.factor(registration_start_date):Large+Trt+Large+tenure,data=dta)
  }
  return(results)
}

Reg2=function(dta,month,Size,FE,Length){
  # Identify big agencies ####
  Agencies = Agencies %>% mutate(Small=case_when(n<=quantile(Agencies$n,Size)~1,
                                                 TRUE~0))
  dta =  dta %>% left_join(Agencies,by=c("estate_agent_license_no","estate_agent_name")) %>%
    rename(Cumulative_final=n)

  dtaCount =  dta %>%
    filter(Time_from_register<=31*Length,Time_from_register>=0) %>%
    group_by(registration_no) %>%
    count()

  dtaCount = left_join(dta %>% distinct(registration_no),dtaCount,by="registration_no") %>%
    as_tibble() %>%
    select(registration_no,n)
  dtaCount[is.na(dtaCount)]=0

  dta = left_join(dta,dtaCount,by="registration_no")


  CntInt=interval(ymd("2021-09-01")%m-%months(month),ymd("2021-9-01"))
  TrtInt=interval(ymd("2021-09-01"),ymd("2021-09-01")%m+%months(month))
  dta = dta %>%
    filter(registration_start_date%within%CntInt|registration_start_date%within%TrtInt) %>%
    filter(!floor_date(registration_start_date,"month")%in%c(ymd("2021-07-01"),ymd("2021-08-01")))

  if(FE=="TT"){
    results=fepois(n~as.factor(floor_date(registration_start_date,"bimonth"))*Small+Small|floor_date(registration_start_date,"month"),data=dta%>%distinct(registration_no,.keep_all = TRUE))
  }else if (FE=="FE"){
    results=fepois(n~as.factor(floor_date(registration_start_date,"bimonth")):Small+as.factor(registration_start_date)|estate_agent_name,data=dta %>% distinct(registration_no,.keep_all = TRUE))
  }else if (FE=="2W"){
    results=fepois(n~Small:as.factor(floor_date(registration_start_date,"bimonth"))|estate_agent_name+floor_date(registration_start_date,"month"),data=dta %>% distinct(registration_no,.keep_all = TRUE))
  }else{
    results=felm(n~as.factor(floor_date(registration_start_date,"bimonth")):Small+as.factor(floor_date(registration_start_date,"month"))+Small,data=dta %>% distinct(registration_no,.keep_all = TRUE))
  }
  return(results)
}
