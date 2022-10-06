library(dplyr)
library(tidyr)
library(lubridate)
library(hms)
library(lfe)
library(fixest)
library(modelsummary)

f <- function(x) format(x, digits = 3, nsmall = 2, scientific = FALSE)

#Cumulative Sales
Reg1=function(dta,month,Size,FE){
  # Identify big agencies ####
  Agencies = Agencies %>% mutate(Small=case_when(n<=quantile(Agencies$n,Size)~1,
                                                 TRUE~0))
  dta =  dta %>% left_join(Agencies,by=c("estate_agent_license_no","estate_agent_name"))

  CntInt=interval(ymd("2021-09-01")%m-%months(month),ymd("2021-9-01"))
  TrtInt=interval(ymd("2021-09-01"),ymd("2021-09-01")%m+%months(month))

  dta = dta %>%
    filter(registration_start_date%within%CntInt|registration_start_date%within%TrtInt) %>%
    filter(floor_date(transaction_date,"month")>=floor_date(registration_start_date,"month")) %>%
    mutate(Trt=case_when(registration_start_date%within%TrtInt~1,
                         TRUE~0))
  if(FE=="TT"){
    results=felm(log(final)~Trt*Small+Small+tenure|floor_date(registration_start_date,"month"),data=dta)
  }else if (FE=="FE"){
    results=felm(log(final)~Trt*Small+Trt+tenure|estate_agent_name,data=dta)
  }else if (FE=="2W"){
    results=felm(log(final)~Trt*Small+tenure|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)
  }else{
    results=felm(log(final)~Trt*Small+Trt+Small+tenure,data=dta)
  }
  return(results)
}

#Cumulative Sales in first x months
Reg2=function(dta,month,Size,Length,Output){
  # Identify big agencies ####
  Agencies = Agencies %>% mutate(Trt=case_when(n<=quantile(Agencies$n,Size)~1,
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
    mutate(Post=case_when(registration_start_date%within%TrtInt~1,
                          TRUE~0))
  dta=dta %>% distinct(registration_no,.keep_all = TRUE)
  result1=fepois(n~Trt*Post+Trt+Post,data=dta)
  result2=fepois(n~Trt*Post+Post|estate_agent_name,data=dta)
  result3=fepois(n~Trt*Post+Trt|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)
  result4=fepois(n~Trt*Post|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)

  results=modelsummary(list("No_FE"=result1,"FE"=result2,"TT"=result3,"2W"=result4),stars=TRUE,
                       fmt=f,output=Output, gof_omit = 'DF|Deviance|R2|AIC|BIC|FE|Std.Errors')
  return(results)
}


#Testing for size specific effects
Reg3=function(dta,month,Length,Output){
  # Identify big agencies ####
  Agencies = cbind(Agencies,ntile(Agencies$n,10)) %>% rename(Quant=4)
  dta =  dta %>% left_join(Agencies,by=c("estate_agent_license_no","estate_agent_name")) %>%
    rename(Agency_Size=n) %>%
    mutate(Trt=case_when(Agency_Size%in%c(1:10)~1,
                         TRUE~0))

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
    mutate(Post=case_when(registration_start_date%within%TrtInt~1,
                          TRUE~0))

  dta=dta %>% distinct(registration_no,.keep_all = TRUE)
  result1=fepois(n~Post:Trt+Post:Trt:Agency_Size+Trt+Post+Agency_Size,data=dta)
  result2=fepois(n~Post:Trt+Post:Trt:Agency_Size+Trt+Agency_Size|floor_date(registration_start_date,"month"),data=dta)
  result3=fepois(n~Post:Trt+Post:Trt:Agency_Size+Post+Agency_Size|estate_agent_name,data=dta)
  result4=fepois(n~Post:Trt+Post:Trt:Agency_Size+Agency_Size|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)

  results=modelsummary(list(result1,result2,result3,result4),stars=TRUE,output=Output, gof_omit = 'DF|Deviance|R2|AIC|BIC|FE|Std.Errors')
  return(results)
}

#Sellers
Reg4=function(dta,month,Size,FE,Length,Output){
  # Identify big agencies ####
  Agencies = Agencies %>% mutate(Trt=case_when(n<=quantile(Agencies$n,Size)~1,
                                               TRUE~0))
  dta =  dta %>% left_join(Agencies,by=c("estate_agent_license_no","estate_agent_name")) %>%
    rename(Cumulative_final=n)

  dtaCount =  dta %>%
    filter(Time_from_register<=31*Length,Time_from_register>=0) %>%
    group_by(registration_no,represented) %>%
    count() %>%
    pivot_wider(names_from = represented,values_from = n) %>%
    replace(is.na(.),0) %>%
    rowwise() %>%
    mutate(TOTAL=sum(SELLER,BUYER,LANDLORD,TENANT))

  dtaCount = left_join(dta %>% distinct(registration_no),dtaCount,by="registration_no") %>%
    as_tibble() %>%
    select(registration_no,SELLER,BUYER,LANDLORD,TENANT,TOTAL)
  dtaCount[is.na(dtaCount)]=0

  dta = left_join(dta,dtaCount,by="registration_no")


  CntInt=interval(ymd("2021-09-01")%m-%months(month),ymd("2021-9-01"))
  TrtInt=interval(ymd("2021-09-01"),ymd("2021-09-01")%m+%months(month))

  dta = dta %>%
    filter(registration_start_date%within%CntInt|registration_start_date%within%TrtInt) %>%
    mutate(Post=case_when(registration_start_date%within%TrtInt~1,
                          TRUE~0))

  dta=dta %>% distinct(registration_no,.keep_all = TRUE)
  if(FE=="TT"){
    results1=fepois(SELLER~Trt*Post+Trt|floor_date(registration_start_date,"month"),data=dta)
    results2=fepois(BUYER~Trt*Post+Trt|floor_date(registration_start_date,"month"),data=dta)
    results3=fepois(LANDLORD~Trt*Post+Trt|floor_date(registration_start_date,"month"),data=dta)
    results4=fepois(TENANT~Trt*Post+Trt|floor_date(registration_start_date,"month"),data=dta)
  }else if (FE=="FE"){
    results1=fepois(SELLER~Trt*Post+Post|estate_agent_name,data=dta)
    results2=fepois(BUYER~Trt*Post+Post|estate_agent_name,data=dta)
    results3=fepois(LANDLORD~Trt*Post+Post|estate_agent_name,data=dta)
    results4=fepois(TENANT~Trt*Post+Post|estate_agent_name,data=dta)
  }else if (FE=="2W"){
    results1=fepois(SELLER~Trt*Post|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)
    results2=fepois(BUYER~Trt*Post|estate_agent_name+floor_date(registration_start_date,"month"),data=dta)
  }else{
    results1=fepois(SELLER~Trt*Post+Post+Small,data=dta)
    results2=fepois(BUYER~Trt*Post+Post+Small,data=dta)
    results3=fepois(LANDLORD~Trt*Post+Post+Small,data=dta)
    results4=fepois(TENANT~Trt*Post+Post+Small,data=dta)
  }
  result=modelsummary(list("SELLER"=results1,"BUYER"=results2),stars = TRUE,output = Output, gof_omit = 'DF|Deviance|R2|AIC|BIC')
  return(result)
}
