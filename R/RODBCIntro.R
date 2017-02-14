library(RODBC)
sessionInfo()

##############################################################################################
##############################################################################################
##############################################################################################
# This script provides a bunch of sample code for using RODBC to connect to PacFIN and
# pull PacFIN data into R

##############################################################################################
##############################################################################################
##############################################################################################


##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#First Connection:

#don't worry about this first part...I just need to get my PacFIN credentials in here without
# making them visiable
pf.cred <- read.csv("pflogon.csv")

#Identify ODBC connections
odbcDataSources(type=c('all'))

#connect to PacFIN
channel<- odbcConnect(dsn="pacfin",uid=paste(pf.cred$UID),pw=paste(pf.cred$PW),believeNRows=FALSE)
sqlTables(channel,schema="PACFIN_MARTS")
close(channel)
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################


###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
#get total landings of IFQ sablefish and aggregate by year, major port area, and gear type

channel<- odbcConnect(dsn="pacfin",uid=paste(pf.cred$UID),pw=paste(pf.cred$PW),believeNRows=FALSE)
sabl <- sqlQuery(channel,"select PACFIN_PORT_CODE,  PACFIN_YEAR, PACFIN_GROUP_GEAR_CODE, sum(LANDED_WEIGHT_LBS)
                 from PACFIN_MARTS.COMPREHENSIVE_FT
                 where IS_IFQ_LANDING='T' and PACFIN_PORT_CODE in ('MRO','AVL','MNT','MOS','PRN','SF')
                 group by PACFIN_PORT_CODE, PACFIN_YEAR, PACFIN_GROUP_GEAR_CODE")
close(channel)

sabl <- tbl_df(sabl)
sabl
names(sabl) <- c('port','year','gear','lbs')

#form a port aggregate and weed out some trivial gear types
sabl.cc <- sabl %>% filter(gear %in% c('HKL','TWL','POT')) %>% 
          mutate(portgroup=ifelse(port %in% c('MRO','AVL'),'Morro Bay',
                                  ifelse(port %in% c('MNT','MOS'),'Monterey Bay',
                                         ifelse(port=='PRN', 'Half Moon Bay','San Francisco')))) %>%
          group_by(portgroup,gear,year) %>%
          summarise(lbs=sum(lbs,na.rm=T)) %>%
            ungroup() %>%
          mutate(portgroup=factor(portgroup,levels=c('San Francisco','Half Moon Bay','Monterey Bay','Morro Bay'))) %>%
          mutate(gear=factor(gear,levels=c('HKL','POT','TWL')))

#plot CA Central Coast 
ggplot(sabl.cc,
       aes(x=year,y=lbs,fill=gear,order=gear)) + 
  geom_bar(stat='identity',position='stack') + facet_wrap(~portgroup) +
  theme_bw()
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
# A Query with dates...

#the Oracle equivilent of 'DATEPART'
channel<- odbcConnect(dsn="pacfin",uid=paste(pf.cred$UID),pw=paste(pf.cred$PW),believeNRows=FALSE)
sqlQuery(channel,"select count(FTID) from PACFIN_MARTS.COMPREHENSIVE_FT 
where extract(year from LANDING_DATE) > 2015")
close(channel)

#The query above should be the same as this:
channel<- odbcConnect(dsn="pacfin",uid=paste(pf.cred$UID),pw=paste(pf.cred$PW),believeNRows=FALSE)
sqlQuery(channel,"select count(FTID) from PACFIN_MARTS.COMPREHENSIVE_FT 
         where LANDING_YEAR > 2015")
close(channel)
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################



###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# Logbook trip distances

#-------------------------------------------------------
#lat/long coordinates for each port and the key to match
# port identifiers between observer data and PacFIN data
port_codes <- read.csv("data/lbk_port_codes.csv")
#-------------------------------------------------------

#--------------------------------------------------------
#set-up port coordinates - we set this up by departure and
# return port because we will need lat/long for both in order
# to approximate the trip distance

dport_codes <- data.frame(d_port=port_codes$OBS_port,dport_lat=port_codes$LAT,dport_long=port_codes$LONG)
dport_codes <- dport_codes[!duplicated(dport_codes),]
dport_codes <- dport_codes[which(dport_codes$d_port!=""),]

rport_codes <- data.frame(r_port=port_codes$OBS_port,rport_lat=port_codes$LAT,rport_long=port_codes$LONG,rPCID=port_codes$PCID)
rport_codes <- rport_codes[!duplicated(rport_codes),]
rport_codes <- rport_codes[which(rport_codes$r_port!=""),]
#----------------------------------------------------------

#------------------------------------------------------------
#pull the tow level data from logbooks and join with logbook trip info
channel <- odbcConnect(dsn="pacfin",uid=paste(pf.cred$UID),pw=paste(pf.cred$PW),believeNRows=FALSE)
tows <- sqlQuery(channel,"select LBK_TOW.TRIP_ID, LBK_TOW.TOWNUM, LBK_TOW.SET_LAT, LBK_TOW.SET_LONG, LBK_TOW.AGID, LBK_TRIP.RPORT, LBK_TRIP.DPORT from PACFIN.LBK_TOW inner join PACFIN.LBK_TRIP on LBK_TOW.TRIP_ID=LBK_TRIP.TRIP_ID")
close(channel)
#-------------------------------------------------------------

#------------------------------------------------------------
#fix longitude values
tows <- tbl_df(tows) %>% mutate(SET_LONG = SET_LONG * -1)
#-----------------------------------------------------------

#----------------------------------------------------------------------------
#before joining the port codes data frame with the tows data frame we have to fix the Astoria code because it's
# '02' which often gets read as 2:
port_codes$LBK_PORT <- as.character(port_codes$LBK_PORT)
port_codes$LBK_PORT[which(port_codes$AGID=='O' & port_codes$PCID=='AST')] <- '02'
#------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------
d_port_codes <- port_codes[,c("LBK_PORT","AGID","LAT","LONG")]
names(d_port_codes) <- c("DPORT","AGID","LAT","LONG")

tows <- tows %>% left_join(d_port_codes,by=c('DPORT','AGID'))
names(tows) <- c('TRIP_ID','TOWNUM','SET_LAT','SET_LONG','AGID','RPORT','DPORT','DPORT_LAT','DPORT_LONG')

r_port_codes <- port_codes[,c("LBK_PORT","AGID","LAT","LONG")]
names(r_port_codes) <- c("RPORT","AGID","LAT","LONG")

tows <- tows %>% left_join(r_port_codes,by=c('RPORT','AGID'))
names(tows) <- c('TRIP_ID','TOWNUM','SET_LAT','SET_LONG','AGID','RPORT','DPORT',
                 'DPORT_LAT','DPORT_LONG','RPORT_LAT','RPORT_LONG')
#-----------------------------------------------------------------------------

#------------------------------------------------------------------------------
#calculate distance per trip by connecting tow sets

#first a diagnostic...number of trips with no good tow locations and number of 
# trips with number of good tow set locations = number of tows
tows.tmp <- tows %>% group_by(TRIP_ID) %>% mutate(bad_pos=ifelse(is.na(SET_LAT),1,0),good_pos=1-bad_pos) %>%
  summarise(townum=max(TOWNUM),good_pos=sum(good_pos),bad_pos=sum(bad_pos))

tows.tmp %>% mutate(all.good=ifelse(good_pos==townum,1,0)) %>% ungroup() %>% summarise(all.good=sum(all.good))

length(tows.tmp$good_pos[tows.tmp$good_pos==0])

dtemp.fn <- function(x,y){
  p1 <- (x*pi)/180
  p2 <- (y*pi)/180
  a <- sin((p2[1]-p1[1])*0.5)*sin((p2[1]-p1[1])*0.5) + sin((p2[2]-p1[2])*0.5)*sin((p2[2]-p1[2])*0.5)*cos(p1[1])*cos(p2[1])
  c <- 2*atan2(sqrt(a),sqrt(1-a))
  d <- c*6371
  return(d)
}


#filter the data set to include only good tow set locations
tow.locs <- tows %>% filter(!is.na(SET_LAT)) %>% filter(!is.na(SET_LONG)) %>% filter(!is.na(DPORT_LAT)) %>%
  filter(!is.na(RPORT_LAT)) %>% filter(!is.na(DPORT_LONG)) %>% filter(!is.na(RPORT_LONG)) %>%
  filter(SET_LAT > 20 & SET_LONG < 20) %>% 
  arrange(TRIP_ID,TOWNUM) 

#get the distance from departure port to first tow set
dstart <- tow.locs %>% arrange(TRIP_ID,TOWNUM) %>% group_by(TRIP_ID) %>% filter(row_number()==1) %>%
  mutate(dstart=dtemp.fn(x=c(DPORT_LONG,DPORT_LAT),y=c(SET_LONG,SET_LAT)))

quantile(dstart$dstart,probs=c(0.001,0.25,0.5,0.75,0.99))

# get distance from last tow to return port
dend <- tow.locs %>% arrange(TRIP_ID,TOWNUM) %>% group_by(TRIP_ID) %>% filter(row_number()==n()) %>%
  mutate(dend=dtemp.fn(x=c(RPORT_LONG,RPORT_LAT),y=c(SET_LONG,SET_LAT)))

# get distance between all tow sets on the trip...in order to do this one we need the trip to have
# at least two good tow set positions...for some reason this doesn't like my user defined function so
# we'll just hardcode the haversine distance
tow.dist <- tow.locs %>% select(TRIP_ID, TOWNUM, SET_LAT, SET_LONG, AGID, DPORT, RPORT) %>%
  group_by(TRIP_ID) %>% 
  arrange(TRIP_ID, TOWNUM) %>%
  mutate(ntows=n_distinct(TOWNUM),
         lat.now=(SET_LAT*pi)/180, 
         long.now=(SET_LONG*pi)/180,
         lag.long=(lag(SET_LONG)*pi)/180,
         lag.lat=(lag(SET_LAT)*pi/180)) %>%
  mutate(a=sin((long.now-lag.long)*0.5)*sin((long.now-lag.long)*0.5) + sin((lat.now-lag.lat)*0.5)*sin((lat.now-lag.lat)*0.5)*cos(lag.long)*cos(long.now),
         c= 2*atan2(sqrt(a),sqrt(1-a)),
         d=c*6371) %>%
  summarise(d=sum(d,na.rm=T))

#put the distances together
d <- dstart %>% select(TRIP_ID,dstart) %>% 
  inner_join(dend,by=c('TRIP_ID')) 
d <- d %>% 
  inner_join(tow.dist,by=c('TRIP_ID')) %>%
  mutate(trip.dist=dstart+dend+d)

#quick plot just to see how things look
plot.df <- d %>% filter(DPORT %in% c('AST','02','223')) %>% 
            mutate(DPORT.new = ifelse(DPORT %in% c('AST','02'),'Astoria',
                                      'Fort Bragg'))
ggplot(plot.df,aes(x=trip.dist)) + geom_histogram(color='black',fill='blue',alpha=0.3) + 
  facet_wrap(~DPORT.new) + theme_bw()

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################



########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
# A quick comparison of filtering big tables inside R vs. filter through SQL

t <- Sys.time()
channel<- odbcConnect(dsn="pacfin",uid=paste(pf.cred$UID),pw=paste(pf.cred$PW),believeNRows=FALSE)
ft <- sqlQuery(channel, "select FTID, PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, LANDED_WEIGHT_LBS from PACFIN_MARTs.COMPREHENSIVE_FT where LANDING_YEAR > 2013")
close(channel)
Sys.time() - t

#get unique gear codes
unique(ft$PACFIN_GEAR_CODE)

#filter data frame
ft <- tbl_df(ft) %>% filter(!PACFIN_GEAR_CODE %in% c('RLT','BTR','MDT','TRL'))



t <- Sys.time()
channel<- odbcConnect(dsn="pacfin",uid=paste(pf.cred$UID),pw=paste(pf.cred$PW),believeNRows=FALSE)
ft <- sqlQuery(channel, "select FTID, PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, LANDED_WEIGHT_LBS 
               from PACFIN_MARTs.COMPREHENSIVE_FT where LANDING_YEAR > 2013 and PACFIN_GEAR_CODE in ('RLT','BTR','MDT','TRL')")
close(channel)
Sys.time() - t
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################



