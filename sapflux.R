###############################################
### Sapflow calculations for bucktorn       ###
### removal experiment. The following data  ###
### frames of interest are outlined below   ###
###############################################
###############################################
### ash.tree & buckthorn.tree:              ###  
### raw measurements and calculations       ###
###############################################
### ash.Flow & buckthorn.Flow:              ###
### L s-1 flow rate at each 15 min increment###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
### ash.Flow.m2 & buckthorn.Flow.m2         ###
### L m-2 s-1 flow rater per m2 of canopy   ###
### at each 15 min increment                ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
### buckthorn.L.m2.day & ash.L.m2.day       ###
### total water use in L per day per m2 of  ###
### canopy leaf area                        ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################
### buckthorn.L.day & ash.L.day             ###
### total water use in L per day per tree   ###
### with average & spread over experimental ###
### removal groups (C & R)                  ###
###############################################






#### libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)


##############################
#### data directory #### ----
userNumber <- 3
#sapflow and sensor data parent directory
dirData <- c("E:/Google Drive/research/projects/campus/buckthorn/sapflux",#windows office
             "/Users/hkropp/Google Drive/research/projects/campus/buckthorn/sapflux",
             "/Users/emilybenson/Desktop/sapflux") # teaching mac
DirMeter <- c("E:/Google Drive/research/projects/Data/campus_weather/METER/",
                "/Users/hkropp/Google Drive/research/projects/Data/campus_weather/METER/",
                "/Users/emilybenson/Desktop/campus_weather/METER")

DirTOMST <- c(paste0("c:/Google Drive/research/projects/Data/campus_weather/TOMST"),
              paste0("E:/Google Drive/research/projects/Data/campus_weather/TOMST"),
              paste0("/Users/emilybenson/Desktop/campus_weather/TOMST")) 

#sapflow download date for file
sversion <- "09_24_2021"

##############################
#### read in data ###### ----
#dT sapflow
sapRaw <- read.csv(paste0(dirData[userNumber],"/campbell/",sversion,"/Sapflow_TableDT.dat"),
                    header=FALSE,skip=4,na.strings=c("NAN"))
#column names
sapInfo <- read.csv(paste0(dirData[userNumber],"/campbell/",sversion,"/Sapflow_TableTC.dat"),
                   header=FALSE,skip=4,na.strings=c("NAN"))

#green ash allometrry
greenwood <- read.csv(paste0(dirData[userNumber],"/green ash olson paper measurements.csv"))
#sapwood allometry
buckthornSW <- read.csv(paste0(dirData[userNumber],"/buckthorn_allometry_info.csv"))
#SLA
buckthornSLA <- read.csv(paste0(dirData[userNumber],"/leaf area.csv"))
#buchthorn dbh and leaf allom
buckthornLA <- read.csv(paste0(dirData[userNumber],"/buckthorn_leaf_allom.csv"))
#list of buckthorn removed with dbh
buckthornRemove <- read.csv(paste0(dirData[userNumber],"/buckthorn_dbh.csv"))
#read in sensor info

sensors <- read.csv(paste0(dirData[userNumber],"/sensors_meta.csv"))


##############################
#### organize sap flow ----
heaterv <- data.frame(date =  ymd_hms(sapInfo[,1]),
                      ht1 = sapInfo[,165],
                      ht2 = sapInfo[,166])
heaterv$year <- year(heaterv$date)
heaterv$doy <- yday(heaterv$date)
heaterv$hour <- hour(heaterv$date)+(minute(heaterv$date)/60)

#remove unused sensor locations
datSap <- sapRaw[,1:18]
#rename columns
colnames(datSap ) <- c("date","record",paste0("dT",seq(1,16)))
#parse date
datSap$dateF <- ymd_hms(datSap$date)
datSap$year <- year(datSap$dateF)
datSap$doy <- yday(datSap$dateF)
datSap$hour <- hour(datSap$dateF)+(minute(datSap$dateF)/60)
datSap$DD <- datSap$doy + (datSap$hour/24)

##############################
#### sapwood depth Allometry ----


# check sapwood depth for sensors


# buckthorn sapwood allometry
# dbh vs sapwood 
# plot(buckthornSW$DBH.cm, buckthornSW$Sapwood.mm/10, pch=19)
# linear regression sap
bsap.lm <- lm(buckthornSW$Sapwood.mm/10 ~ buckthornSW$DBH.cm )
summary(bsap.lm)

bsap.calc <- mean( buckthornSW$Sapwood.mm/10)


#dbh vs bark thickness
# plot(buckthornSW$DBH.cm, buckthornSW$bark.mm, pch=19)
# bbark.lm <- lm(buckthornSW$bark.mm/10 ~ buckthornSW$DBH.cm )
# summary(bbark.lm)
# bark relationship not significant
# assume mean
bbark.calc <- mean( buckthornSW$bark.mm/10)


# ash sapwood depth allometry from Zeima Kassahun, Heidi J. Renninger 2021 Ag & Forest Met
sensors$sd.cm <- ifelse(sensors$Type == "Ash", #if sensors is ash
                        -36.33 + (44.28*(1-exp(-0.1306*sensors$DBH.cm))),#allometry
                        bsap.calc)#if buckthorn fill place with 1 cm placeholder until allometry is fully measured




##############################
#### dT to v calcs ##### ----

# organize data for easier calculations
tabledt <- datSap


dtAll <- data.frame(date= rep(tabledt$date, times = 16), 
                    doy = rep(tabledt$doy, times = 16),
                    hour = rep(tabledt$hour, times = 16),
                    DD = rep(tabledt$DD, times = 16),
                    sensor = rep(seq(1,16), each = nrow(tabledt)), 
                    dT = c(tabledt[,3],
                           tabledt[,4],
                           tabledt[,5],
                           tabledt[,6],
                           tabledt[,7],
                           tabledt[,8],
                           tabledt[,9],
                           tabledt[,10],
                           tabledt[,11],
                           tabledt[,12],
                           tabledt[,13],
                           tabledt[,14],
                           tabledt[,15],
                           tabledt[,16],
                           tabledt[,17],
                           tabledt[,18]))

# filter out unreliable data due to voltage regulator issues

#moved heaters to more reliable first regulator on july 2 10-10:30 and replaced reculator on July 6 10 am
#ggplot(heaterv, aes(x=date,y=ht1))+ 
#  geom_point()+
#  geom_line()

#ggplot(heaterv, aes(x=date,y=ht2))+ 
#  geom_point()+
#  geom_line()

#indicate which heater
dtAll$htrN <- ifelse(dtAll$sensor <= 8,1,
                     ifelse(dtAll$doy < 183 | dtAll$doy > 187, 2,1))
# heaterv[which(heaterv$ht1 == 0),]

#calculate daily heater sd
heater1sd <- aggregate(heaterv$ht1, by=list(doy=heaterv$doy),FUN="sd")
heater2sd <- aggregate(heaterv$ht2, by=list(doy=heaterv$doy),FUN="sd")
heater1min <- aggregate(heaterv$ht1, by=list(doy=heaterv$doy),FUN="min")
heater2min <- aggregate(heaterv$ht2, by=list(doy=heaterv$doy),FUN="min")

heatersAll <- data.frame(doy= c(heater1sd$doy,heater2sd$doy),
                         htrN=c(rep(1,nrow(heater1sd)),rep(2,nrow(heater2sd))),
                         sd = c(heater1sd$x,heater2sd$x),
                         min=c(heater1min$x,heater2min$x))

ggplot(heatersAll, aes(doy, sd, col=htrN))+
  geom_point()


# join heater info back into dt

dtAll <- left_join(dtAll,heatersAll, by=c("doy","htrN"))



#### QC filter 1    #   
#filter out days when voltage regulator was unreliable
#either too variable or heaters turned off at any point
dtAll <- dtAll[dtAll$sd <= 0.05 & dtAll$min >0,]


#################
#check for dt outliers
# quantile(dtAll$dT, prob=seq(0,1,by=0.001))

#### QC filter 2    #  
#definitely few outliers. 99.5% and above are unusually high
dtAll <- dtAll[dtAll$dT <= quantile(dtAll$dT, prob=0.995),]

#join sensor info into table dt
#make a doy that contains the same night
#so new day actually starts at 5 am not midnight
dtAll$doy5 <- ifelse(dtAll$hour < 5, dtAll$doy-1,dtAll$doy)

night <- dtAll[dtAll$hour < 5|dtAll$hour >= 22,]

#filter night so maximum in day and sensor is provided
maxnight1 <- night %>% 
  group_by(sensor, doy5) %>%
  filter(dT == max(dT),na.rm=TRUE)

#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnight <- maxnight1  %>% 
  group_by(sensor, doy5) %>%
  filter(hour == min(hour),na.rm=TRUE)

ggplot(maxnight, aes(doy5,dT, color=sensor))+
      geom_point()

# isolate max and join back into table
maxJoin <- data.frame(sensor=maxnight$sensor, 
                      doy5=maxnight$doy5,
                      maxDT = maxnight$dT)

# join backinto tabledt
dtCalct1 <- left_join(dtAll, maxJoin, by=c("sensor","doy5"))
# join sensor info
dtCalc <- left_join(dtCalct1 , sensors, by=c("sensor"="SensorID"))

# from clearwater

#sap velocity m s-1 (v)
#v = 0.000119*k^1.231
#flow is F (L s-1) = v* A (m2, sapwood area)

#K= (dTmax - dT)/dT if sensor is fully within sapwood

# otherwise correction is:
# dt sap = (dT - b* Dtmax)/a

# a = proportion of probe in sapwood and b=1-a

dtCalc$a <- ifelse(dtCalc$sd.cm >= 3,1,
                   dtCalc$sd.cm/3)

dtCalc$b <- 1 - dtCalc$a 

dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
dtCalc$K <- (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor
dtCalc$velo <- 0.000119*(dtCalc$K^1.231)


# separate species types
ash <- dtCalc[dtCalc$Type == "Ash",]
buckthorn <- dtCalc[dtCalc$Type == "buckthorn",]

##############################
#### N & S radial check   ----

# compare N & S sensors for ash
sens3 <- data.frame(date = ash$date[ash$sensor == 3],
                    veloN = ash$velo[ash$sensor == 3])
                     
sens4 <- data.frame(date = ash$date[ash$sensor == 4],
                    veloS = ash$velo[ash$sensor == 4])

treeD1 <- inner_join(sens3,sens4, by="date")

#compare N & S sensors for ash
sens12 <- data.frame(date = ash$date[ash$sensor == 12],
                    veloN = ash$velo[ash$sensor == 12])

sens11 <- data.frame(date = ash$date[ash$sensor == 11],
                    veloS = ash$velo[ash$sensor == 11])

treeD2 <- inner_join(sens12,sens11, by="date")

sens15 <- data.frame(date = ash$date[ash$sensor == 15],
                     veloN = ash$velo[ash$sensor == 15])

sens16 <- data.frame(date = ash$date[ash$sensor == 16],
                     veloS = ash$velo[ash$sensor == 16])

treeD3 <- inner_join(sens15,sens16, by="date")

treeDir <- rbind(treeD1,treeD2,treeD3)
#check relationship
azim.rel <- lm(treeDir$veloS ~ treeDir$veloN)
# summary(azim.rel)

#ggplot(treeDir, aes(veloN,veloS))+
  #geom_point()+
  #geom_abline()

#regression does not differ significantly from S=0 + 1*N

# check buckthorn


sens7 <- data.frame(date = ash$date[ash$sensor == 7],
                    veloN = ash$velo[ash$sensor == 7])

sens9 <- data.frame(date = ash$date[ash$sensor == 9],
                    veloS = ash$velo[ash$sensor == 9])

treeB1 <- inner_join(sens3,sens4, by="date")

sens8 <- data.frame(date = ash$date[ash$sensor == 8],
                     veloN = ash$velo[ash$sensor == 8])

sens10 <- data.frame(date = ash$date[ash$sensor == 10],
                     veloS = ash$velo[ash$sensor == 10])

treeB2 <- inner_join(sens8,sens10, by="date")

treeBDir <- rbind(treeB1,treeB2)

azimB.rel <- lm(treeBDir$veloS ~ treeBDir$veloN)
# summary(azimB.rel)

#ggplot(treeBDir, aes(veloN,veloS))+
#  geom_point()+
#  geom_abline()
#regression does not differ significantly from S=0 + 1*N

#use N for final data
ash.tree <- ash[ash$Direction == "N", ]

buckthorn.tree <- buckthorn[buckthorn$Direction == "N", ]


##############################
#### canopy leaf allometry   ----


#Ash allometry from literature
greenwood$sap.area <- greenwood$Sapwood.Volume..ft.3./greenwood$Total.Height..feet
#30.48 cm in 1 foot
greenwood$sap.area.cm <- 30.48*30.48*greenwood$sap.area 
greenwood$dbh.cm <- (greenwood$DBH..inches.*2.54)
greenwood$treeArea <- ((greenwood$dbh.cm /2)^2)*pi

#plot(greenwood$dbh.cm,greenwood$sap.area.cm)

#saparea.reg <- lm(greenwood$sap.area.cm ~ greenwood$dbh.cm)
#summary(saparea.reg)


#sap cm2 = -9.6 + 8.854*DBH cm


##############################
#### Canopy calculations   ----

## sapwood arrea

# ash tree
ash.tree$sap.areacm2 <- -9.6 + 8.854*ash.tree$DBH.cm
#convert sap area to m2
ash.tree$sap.aream2 <- 0.0001*ash.tree$sap.areacm2

# buckthorn

#calculate heartwood
buckthorn.tree$Htwd <- buckthorn.tree$DBH.cm - (bbark.calc*2) - (bsap.calc*2)



#calculate sapwood area

buckthorn.tree$sap.areacm2 <- (pi*(((bsap.calc/2)+(buckthorn.tree$Htwd/2))^2))-(pi*((buckthorn.tree$Htwd/2)^2))
buckthorn.tree$sap.aream2 <-  0.0001*buckthorn.tree$sap.areacm2
## tree leaf area
#meadows paper
#LA (m2) = -66.185 +  6.579*DBH in cm
ash.tree$LA.m2 <- -66.185 +  6.579*ash.tree$DBH.cm

# buckthorn

#Leaf Area/ leaf Mass (cm2 / g)
buckthorn.SLA <- mean(buckthornSLA$area.cm2/buckthornSLA$weight.g)

buckthornLA$Leaf.area <- buckthorn.SLA * buckthornLA$Dry.Leaf.g
buckthornLA$LA.m2 <-  buckthornLA$Leaf.area*0.0001



#check relationship
# lm.log<- lm(log(buckthornLA$LA.m2) ~ log(buckthornLA$DBH.cm))
# summary(lm.log)
# plot(buckthornLA$DBH.cm, buckthornLA$LA.m2)
# plot(log(buckthornLA$DBH.cm), log(buckthornLA$LA.m2))
#regression log(LA (m2)) = -1.058 + 1.828 * log(dbh.cm)

#estimate leaf area in m2
buckthorn.tree$LA.m2 <- exp(-1.058 + (1.828*log(buckthorn.tree$DBH.cm)))

##############################
#### Flow calculations   ----

#flow rate according to clearwater
#F(L s-1) =  v(m s-1)* A (m2)

ash.tree$Flow.m3.s <- ash.tree$velo * ash.tree$sap.aream2

buckthorn.tree$Flow.m3.s <- buckthorn.tree$velo * buckthorn.tree$sap.aream2

#convert to L per secton

ash.tree$Flow.L.s <- ash.tree$Flow.m3.s * 1000

buckthorn.tree$Flow.L.s <- buckthorn.tree$Flow.m3.s * 1000

#normalize by canopy leaf area
ash.tree$Flow.L.m2.s <- ash.tree$Flow.L.s /ash.tree$LA.m2 

buckthorn.tree$Flow.L.m2.s <- buckthorn.tree$Flow.L.s /buckthorn.tree$LA.m2 

#summarize total per day for each tree
#remove NA
ash.treeNN <- ash.tree[is.na(ash.tree$Flow.L.s)==FALSE,]
#calculate total water use by each tree in a day
#total liters used in 15 min period
ash.treeNN$L.p <- ash.treeNN$Flow.L.s* 60 *15
#per canopy area
ash.treeNN$L.p.m2  <- ash.treeNN$L.p/ash.treeNN$LA.m2 

#summarize total per day for each tree
#remove NA
buckthorn.treeNN <- buckthorn.tree[is.na(buckthorn.tree$Flow.L.s)==FALSE,]
#calculate total water use by each tree in a day
#total liters used in 15 min period
buckthorn.treeNN$L.p <- buckthorn.treeNN$Flow.L.s* 60 *15
# per canopy area
buckthorn.treeNN$L.p.m2  <- buckthorn.treeNN$L.p/buckthorn.treeNN$LA.m2 


##############################
#### Summary tables    ----

#summary table
#flow L s every 15 min by treatment
ash.Flow <- ash.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.s),sd=sd(Flow.L.s), n=length(Flow.L.s))
#flow L m-2 leaf s-1 by 15min
ash.Flow.m2 <- ash.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.m2.s),
            sd=sd(Flow.L.m2.s), 
            n=length(Flow.L.m2.s))
#only use time points with at least 3 trees
ash.Flow <- ash.Flow[ ash.Flow$n >=3,]
ash.Flow.m2 <- ash.Flow.m2[ ash.Flow.m2$n >=3,]
ash.Flow$se <- ash.Flow$sd/sqrt(ash.Flow$n)
ash.Flow.m2$se <- ash.Flow.m2$sd/sqrt(ash.Flow.m2$n)

buckthorn.Flow <- buckthorn.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.s),sd=sd(Flow.L.s), n=length(Flow.L.s))

#flow L m-2 leaf s-1 by 15min
buckthorn.Flow.m2 <- buckthorn.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.m2.s),sd=sd(Flow.L.m2.s), n=length(Flow.L.m2.s))

#only use time points with at least 3 trees
buckthorn.Flow <- buckthorn.Flow[ buckthorn.Flow$n >=3,]
buckthorn.Flow.m2 <- buckthorn.Flow.m2[ buckthorn.Flow.m2$n >=3,]
buckthorn.Flow$se <- buckthorn.Flow$sd/sqrt(buckthorn.Flow$n)
buckthorn.Flow.m2$se <- buckthorn.Flow.m2$sd/sqrt(buckthorn.Flow.m2$n)



#total liters per day used by the tree 
ash.L.sens <- ash.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p ), n=length(L.p))

ash.L.sens <- ash.L.sens[ ash.L.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

ash.L.day <- ash.L.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
ash.L.day <-ash.L.day [ash.L.day$n >= 3,]
ash.L.day$se <- ash.L.day$sd/sqrt(ash.L.day$n)

buckthorn.L.sens <- buckthorn.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p ), n=length(L.p))

buckthorn.L.sens <- buckthorn.L.sens[ buckthorn.L.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

buckthorn.L.day <- buckthorn.L.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
buckthorn.L.day <-buckthorn.L.day [buckthorn.L.day$n >= 3,]
buckthorn.L.day$se <- buckthorn.L.day$sd/sqrt(buckthorn.L.day$n)

#total liters per day used by the tree normalized per m2 of leaf area
ash.L.m2.sens <- ash.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p.m2 ), n=length(L.p.m2))

ash.L.m2.sens <- ash.L.m2.sens[ ash.L.m2.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

ash.L.m2.day <- ash.L.m2.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
ash.L.m2.day <-ash.L.m2.day [ash.L.m2.day$n >= 3,]
ash.L.m2.day$se <- ash.L.m2.day$sd/sqrt(ash.L.m2.day$n)

buckthorn.L.m2.sens <- buckthorn.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p.m2 ), n=length(L.p.m2))

buckthorn.L.m2.sens <- buckthorn.L.m2.sens[ buckthorn.L.m2.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

buckthorn.L.m2.day <- buckthorn.L.m2.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
buckthorn.L.m2.day <-buckthorn.L.m2.day [buckthorn.L.m2.day$n >= 3,]
buckthorn.L.m2.day$se <- buckthorn.L.m2.day$sd/sqrt(buckthorn.L.m2.day$n)


rm(list=setdiff(ls(), c("ash.tree","buckthorn.tree",
                        "ash.Flow","buckthorn.Flow",
                        "ash.Flow.m2","buckthorn.Flow.m2",
                        "buckthorn.L.m2.day", "ash.L.m2.day", 
                        "buckthorn.L.day", "ash.L.day",  
                        "weather")))

library(dplyr)

ashcontrol <- ash.L.m2.day %>%
  filter(Removal == "C")

ashremoval <- ash.L.m2.day %>%
  filter(Removal == "R")

ashandbuckthorn <- rbind(ashcontrol, ashremoval, buckthorn.L.m2.day)

#ggplot(buckthorn.Flow, aes(x = doy, y = sd, color = type))+
  geom_point(aes(col = "cadetblue1"))+
  geom_line(aes(col = "cadetblue1"))+
  ggtitle("Buckthorn Sapflow Velocity")+
  xlab("Date")+
  ylab("Velocity (L/m2)")


ggplot(ash.L.m2.day, aes(x = doy, y = mean, color = variable))+
  ggtitle("Ash And Buckthorn Sapflow Velocity")+
  xlab("Date")+
  ylab("Transpiration (L/m2)")+
  stat_summary(fun=mean, data = ashcontrol, aes(group=1, col = "blue"), geom="line") +
  stat_summary(fun=mean, data = ashcontrol, aes(group=1), geom="point", colour="dark blue", size=1, shape=1)+
  stat_summary(fun=mean, data = ashremoval, aes(group=1, col = "red"), geom="line") +
  stat_summary(fun=mean, data = ashremoval, aes(group=1), geom="point", colour="dark red", size=1, shape=1)+
  stat_summary(fun=mean, data = buckthorn.L.m2.day, aes(group=1, col = "green"), geom="line") +
  stat_summary(fun=mean, data = buckthorn.L.m2.day, aes(group=1), geom="point", colour="dark green", size=1, shape=1)+
  scale_color_identity(name = "Tree Type",
                       labels = c("Ash", "Buckthorn", "Ash Removal"),
                       guide = "legend")


meterTable <- read.csv(paste0(DirMeter[userNumber], "/12_z6-10463 12Oct21.csv"), skip=3,header=FALSE)




colnames(meterTable) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                          "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                          "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")


#set up day of year
dateForm <-  ymd_hms(meterTable$Date, tz="America/New_York")
meterTable$dateF <- ymd_hms(meterTable$Date, tz="America/New_York")

meterTable$year <- year(dateForm) 
meterTable$doy <- yday(dateForm)
meterTable$minute <- minute(dateForm)
meterTable$hour <- hour(dateForm)+(minute(dateForm)/60)
meterTable$DD <- meterTable$doy + (meterTable$time/24) 
meterTable$DY <- round(meterTable$year+((meterTable$DD-1)/ifelse(leap_year(meterTable$year),366,365)),6)




MeterMeta <- data.frame(name = c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                                 "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                                 "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp"),
                        units = c("MM/DD/YYYY HH:MM",
                                  "W/m^2","mm","NA","km","degree","m/s","m/s","C",
                                  "kPa","kPa","degree","degree","mm/h","C","kPa","%","mV","kPa","C"))

ashflowweather <- left_join(ash.Flow.m2, meterTable, by = c("doy", "hour"))

#read in tomst data
TomstD <- "07_28_2021"
TomstD2 <- "09_04_2021"


tomstFiles1 <- list.files(paste0(DirTOMST[userNumber],"/",TomstD))
tomstFiles2 <- list.files(paste0(DirTOMST[userNumber],"/",TomstD2))

TOMSTSensor <- data.frame(SN= c(91201802,
                                91200065,
                                94207592,
                                94214744,
                                94214743,
                                94214741,
                                94214742),
                          Height = c(0.25,0.5,0,0,0,0,0),
                          location=c("weather",
                                     "weather",
                                     "weather",
                                     "removal",
                                     "control",
                                     "removal",
                                     "control"
                          ),
                          timeP = c(2,2,2,1,1,2,2))
# 94214743 & 94214744 were moved inside due to firmware issue after 7/28 download. 
# On 8/3 94214742 was setup in control and   94214741 was put in removal around 10 am
# due to weird firmware issue on other sensors
# need to find label


#blank column in new firmware data
#read in files
TMS1p2 <-  read.csv(paste0(DirTOMST[user],"/",TomstD2, "/",tomstFiles2[grep(paste0(TOMSTSensor$SN[3]),tomstFiles2)]),
                    sep=";",header=FALSE)[,1:9]
TMS2p2 <-  read.csv(paste0(DirTOMST[user],"/",TomstD2, "/",tomstFiles2[grep(paste0(TOMSTSensor$SN[6]),tomstFiles2)]),
                    sep=";",header=FALSE)[,1:9]
TMS3p2 <-  read.csv(paste0(DirTOMST[user],"/",TomstD2, "/",tomstFiles2[grep(paste0(TOMSTSensor$SN[7]),tomstFiles2)]),
                    sep=";",header=FALSE)[,1:9]

TMS2p1 <-  read.csv(paste0(DirTOMST[user],"/",TomstD, "/",tomstFiles1[grep(paste0(TOMSTSensor$SN[4]),tomstFiles1)]),
                    sep=";",header=FALSE)
TMS3p1 <-  read.csv(paste0(DirTOMST[user],"/",TomstD, "/",tomstFiles1[grep(paste0(TOMSTSensor$SN[5]),tomstFiles1)]),
                    sep=";",header=FALSE)
#tms temps:  -6, +2 and +15cm
TMScols <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")

colnames(TMS1p2) <- TMScols
colnames(TMS2p2) <- TMScols
colnames(TMS3p2) <- TMScols
colnames(TMS2p1) <- TMScols
colnames(TMS3p1) <- TMScols

#new tomst sensors use commas instead of periods for decimal

TMS1p2$Tm6 <- as.numeric(gsub("\\,","\\.",TMS1p2$Tm6))
TMS1p2$T2 <- as.numeric(gsub("\\,","\\.",TMS1p2$T2))
TMS1p2$T15 <- as.numeric(gsub("\\,","\\.",TMS1p2$T15))

TMS2p2$Tm6 <- as.numeric(gsub("\\,","\\.",TMS2p2$Tm6))
TMS2p2$T2 <- as.numeric(gsub("\\,","\\.",TMS2p2$T2))
TMS2p2$T15 <- as.numeric(gsub("\\,","\\.",TMS2p2$T15))

TMS3p2$Tm6 <- as.numeric(gsub("\\,","\\.",TMS3p2$Tm6))
TMS3p2$T2 <- as.numeric(gsub("\\,","\\.",TMS3p2$T2))
TMS3p2$T15 <- as.numeric(gsub("\\,","\\.",TMS3p2$T15))



TMS1p2$dateF <- ymd_hm(TMS1p2$date)
TMS1p2$estD <- with_tz(TMS1p2$dateF,tzone="America/New_York" )

TMS2p2$dateF <- ymd_hm(TMS2p2$date)
TMS2p2$estD <- with_tz(TMS2p2$dateF,tzone="America/New_York" )

TMS3p2$dateF <- ymd_hm(TMS3p2$date)
TMS3p2$estD <- with_tz(TMS3p2$dateF,tzone="America/New_York" )

TMS2p1$dateF <- ymd_hm(TMS2p1$date)
TMS2p1$estD <- with_tz(TMS2p1$dateF,tzone="America/New_York" )

TMS3p1$dateF <- ymd_hm(TMS3p1$date)
TMS3p1$estD <- with_tz(TMS3p1$dateF,tzone="America/New_York" )

#omit error flag data
TMS2p1 <- TMS2p1[TMS2p1$errFlag != 16, ]
TMS3p1 <- TMS3p1[TMS3p1$errFlag != 16, ]

# omit data before new sensors were deployed
# original sensors set up at 7/2 10: 25
# but animal destruction impacted removal early on Subset to day where fixed
TMS2p1 <- TMS2p1[TMS2p1$estD >= "2021-07-08 00:00:00",]
TMS3p1 <- TMS3p1[TMS3p1$estD >= "2021-07-08 00:00:00",]


TMS2p2 <- TMS2p2[TMS2p2$estD >= "2021-08-03 10:15:00",]
TMS3p2 <- TMS3p2[TMS3p2$estD >= "2021-08-03 10:15:00",]

control <- rbind(TMS3p1,TMS3p2)
removal <- rbind(TMS2p1,TMS2p2)
weather <- TMS1p2

weather$location <- rep("weather",nrow(weather))
removal$location <- rep("removal",nrow(removal))
control$location <- rep("control",nrow(control))

# loam calculation
weather$SM.cor <- (-0.00000005*(weather$SM^2)) + (0.000398*weather$SM) -0.291
removal$SM.cor <- (-0.00000005*(removal$SM^2)) + (0.000398*removal$SM) -0.291
control$SM.cor <- (-0.00000005*(control$SM^2)) + (0.000398*control$SM) -0.291


TMSbind <- rbind(weather,removal,control)
removalT <- removal
controlT <- control

TMSbind$year <- year(TMSbind$estD)
TMSbind$doy <-  yday(TMSbind$estD)
TMSbind$DD <- yday(TMSbind$estD) + ((hour(TMSbind$estD)+(minute(TMSbind$estD)/60))/24)

TMSsub <- TMSbind[TMSbind$doy >= 191 & TMSbind$year == 2021,]

rm(TMS2p1)
rm(TMS1p2) 
rm(TMS2p2)
rm(TMS3p1)
rm(TMS3p2)
#trying to get the average vpd per day for control and removal to compare the two and see it over time
subplot2 <- ashflowweather %>%
  group_by(doy, Removal) %>%
  summarise(VPDmax = ifelse(max(VPD, na.rm = TRUE)== -Inf, NA, max(VPD, na.rm = TRUE)),
            VPDave = ifelse(mean(VPD, na.rm = TRUE)== -Inf, NA, mean(VPD, na.rm = TRUE)))

          
ggplot(subplot2, aes(x = doy, y = VPDave, color = Removal))+
  geom_point()+
  ggtitle("VPD Daily Averages")

ggplot(subplot2, aes(x = doy, y = VPDmax, color = Removal))+
  geom_point()+
  ggtitle("VPD Daily Max")

subplot1 <- ashflowweather %>%
  filter(doy == 213)

ggplot(subplot1, aes(x = VPD, y = mean, color = Removal))+
  geom_point()+
  ggtitle("Day 213")

dailyweather <- meterTable %>%
  group_by(doy) %>%
  summarise(VPDmax = ifelse(max(VPD, na.rm = TRUE)== -Inf, NA, max(VPD, na.rm = TRUE)),
            VPDave = ifelse(mean(VPD, na.rm = TRUE)== -Inf, NA, mean(VPD, na.rm = TRUE)),
            nVPD = length(na.omit(VPD)),
            SolRadmax = ifelse(max(SolRad, na.rm = TRUE)== -Inf, NA, max(SolRad, na.rm = TRUE)))
                            
ashweather <- left_join(ash.L.m2.day, dailyweather, by = "doy")


ggplot(ashweather, aes(x = VPDmax, y = mean, color = Removal))+
  geom_point()


ggplot(ashweather, aes(x = doy, y = mean, color = Removal))+
  geom_point()

ggplot(ashweather[ashweather$Removal=="C",], aes(x = doy, y = VPDmax))+
  geom_point()+
  geom_line()

smashweather <- left_join(ash.L.m2.day, TMSsub, by = "doy")

ggplot(smashweather, aes(x = SM, y = mean, color = Removal))+
  geom_point()

smweather <- left_join(TMSsub, dailyweather, by = "doy")

ggplot(smweather, aes(x = VPDmax, y = SM, color = "Legend"))+
  geom_point()

#tomstdata <- read.table(paste0(dirWeather[userNumber],"/data_94214742_1.csv"), sep=";", header = FALSE)
#TMScols <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")
#weatherdata <- read.csv(paste0(dirWeather[userNumber],"/12_z6-10463 12Oct21.csv"))

