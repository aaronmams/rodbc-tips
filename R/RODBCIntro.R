library(RODBC)
sessionInfo()

#connect to PacFIN
channel<- odbcConnect(dsn="pacfin",uid='amamula',pw='mam2pac$',believeNRows=FALSE)
sqlTables(channel,schema="PACFIN_MARTS")
close(channel)


