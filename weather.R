#libraries
library(lubridate)
library(dplyr)
library(ggplot2)


########ggplot2############################
########## User inputs -----------

#### Setting up directories 

# Creating user numbers for each person
UsersAll <- data.frame(userID = c(1,2,3), userName=c("Student lab","Professor Kropp", "emilybenson"))


#most recent tomst download
#assumes downloading all data

#Need to read in both files for control and removal plots due to sensor swap


TomstD <- "07_28_2021"
TomstD2 <- "09_04_2021"

# File path for meter data
DirMeter <- c("c:/Google Drive/research/projects/Data/campus_weather/METER/CSV/12_z6-10463 12Oct21.csv",
              "E:/Google Drive/research/projects/Data/campus_weather/METER/CSV/12_z6-10463 12Oct21.csv",
              "/Users/emilybenson/Desktop/campus_weather/METER/CSV/12_z6-10463 12Oct21.csv")



DirTOMST <- c(paste0("c:/Google Drive/research/projects/Data/campus_weather/TOMST"),
              paste0("E:/Google Drive/research/projects/Data/campus_weather/TOMST"),
              paste0("/Users/emilybenson/Desktop/campus_weather/TOMST")) 

# Select user - change if needed
user <- 3

############################
########## Meter -----------

#### Set up metadata and initial tables ####
#read in first file

#get all files


meterTable <- read.csv(paste0(DirMeter[user]), skip=3,header=FALSE)




colnames(meterTable) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                          "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                          "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")


#set up day of year
dateForm <-  ymd_hms(meterTable$Date, tz="America/New_York")

meterTable$year <- year(dateForm) 
meterTable$doy <- yday(dateForm)
meterTable$hour <- hour(dateForm)
meterTable$minute <- minute(dateForm)
meterTable$time <- hour(dateForm)+(minute(dateForm)/60)
meterTable$DD <- meterTable$doy + (meterTable$time/24) 
meterTable$DY <- round(meterTable$year+((meterTable$DD-1)/ifelse(leap_year(meterTable$year),366,365)),6)



MeterMeta <- data.frame(name = c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                                 "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                                 "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp"),
                        units = c("MM/DD/YYYY HH:MM",
                                  "W/m^2","mm","NA","km","degree","m/s","m/s","C",
                                  "kPa","kPa","degree","degree","mm/h","C","kPa","%","mV","kPa","C"))





############################
########## QA/QC -----------
#add in data flags and QAQC here



############################
########## TOMST-----------
#get files

tomstFiles1 <- list.files(paste0(DirTOMST[user],"/",TomstD))
tomstFiles2 <- list.files(paste0(DirTOMST[user],"/",TomstD2))

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
weather$SM.cor <- (-0.00000002*(weather$SM^2)) + (0.0003*weather$SM) -0.2062
removal$SM.cor <- (-0.00000002*(removal$SM^2)) + (0.0003*removal$SM) -0.2062
control$SM.cor <- (-0.00000002*(control$SM^2)) + (0.0003*control$SM) -0.2062


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

ggplot(TMSbind[TMSbind$doy >= 189 & TMSbind$year == 2021,], aes(x = DD, y = SM.cor, color = as.factor(location)))+
  geom_line()+
  ggtitle("Soil Moisture Levels (Raw)")+
  xlab("Date")+
  ylab("SM")

ggplot(TMSsub, aes(x = DD, y = Tm6, color = Legend))+
  geom_line(aes(col = "Temperature"))+
  ggtitle("Temp Levels 6 inches Below")+
  xlab("Date")+
  ylab("TEMP")

meterTable2 <- meterTable[meterTable$doy >= 191 & meterTable$year == 2021,]

meterTable2 <- na.omit(meterTable2)

ggplot(meterTable2, aes(x = DD, y = Precip, color = Legend))+
  geom_line(aes(color = "Precipitation"))+
  scale_color_manual(values=c('Blue'))+
  ggtitle("Precipitation Levels")+
  xlab("Date")+
  ylab("Precipitation (mm)")

ggplot(meterTable2, aes(x = DD, y = VaporPr, color = Legend))+
  geom_line(aes(color = "Vapor Pressure"))+
  scale_color_manual(values=c('purple'))+
  ggtitle("Vapor Pressure Levels")+
  xlab("Date")+
  ylab("VP (kPa)")


