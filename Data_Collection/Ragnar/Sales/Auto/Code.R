library(cronR)
cmd = cron_rscript("/home/keitaro2/Singapore_Realtor/Data_Collection/Sales/Run/Code.R")

cron_add(command=cmd,frequency = "/30 * * * *", at="17:00")
y