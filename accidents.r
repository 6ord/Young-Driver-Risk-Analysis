# install.packages("sparklyr")
library(sparklyr)

# install.packages("data.table")
# library(data.table)
# Tried data.table, no noticable difference in run times. Code is simpler though.

# install.packages("pdftools")
library(pdftools)

# spark_install(version = "2.1.0")
# devtools::install_github("rstudio/sparklyr")


# sc <- spark_connect(master = "local")
# 
# sp.accid <- spark_read_csv(sc,name = "accid_tbl"
#                            ,path = "F:/Marsh/canadian-car-accidents-19942014/NCDB_1999_to_2014.csv"
#                            ,header = TRUE
#                           #,stringsAsFactors = FALSE
#                             )
# select(accid_tbl,C_YEAR)

rm(list=ls())

# IMPORTING DATA

setwd('F:\\Ryerson\\CKME136_Capstone\\repository')
accid <- #as.data.table(
                       read.csv(".\\canadian-car-accidents-19942014\\NCDB_1999_to_2014.csv"
                                , header = TRUE
                                , stringsAsFactors = FALSE
                                )
        #               )


###########################################################################
##################### Section 1: DATA DICTIONARY ##########################
###########################################################################

dataDictionary <- pdf_text(".\\canadian-car-accidents-19942014\\drivingLegend.pdf")

# Capture first page of PDF
# 
dd.defitn <- unlist(strsplit(dataDictionary,'\n')[1])
#dd.values <- unlist(strsplit(dataDictionary,'\n')[2:11])

# Remove sub-headings for a clean table
# 
dd.defitn <- c(dd.defitn[3:14],
               dd.defitn[16:18],
               dd.defitn[20:26])

# Initialize data definition data frame, based on the first page of PDF above,
# with just attribute code and attribute name.
# 
defitn.tbl <- as.data.frame(cbind(sapply(dd.defitn,function(x){gsub(' ','',substr(x,1,8))}),
                                  sapply(dd.defitn,function(x){substr(x,44,nchar(x)-1)})
                                  ))
rm(dd.defitn)
row.names(defitn.tbl) <- NULL
colnames(defitn.tbl) <- c('attribute','description')

# Add columns to data definition dataframe defitn.tbl:
# - unique values under each attribute
# - number of missing values ('U' and 'X') under each attribute
# - attribute type
# 
missingVal <- c('UU','XX','U','X')
defitn.tbl$values <- ''
for (i in 1:22){defitn.tbl$values[i] <- unique(accid[i])
                defitn.tbl$numNA[i] <- sum(accid[,i] %in% missingVal)
                }
defitn.tbl$attrType <- c('Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Quan-Discrete','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Ordinal','Qual-Nominal','Qual-Ordinal','Qual-Ordinal','Qual-Nominal','Qual-Ordinal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal')

# Check how many drivers are missing age
sum(subset(accid,accid$P_USER=='1')$P_AGE %in% missingVal)
# 150K+

#View(defitn.tbl)

###########################################################################
#####################  Section 2: DATA CLEANING  ##########################
###########################################################################

accid$occurID <- paste(accid$C_YEAR,accid$C_MNTH,accid$C_WDAY,
                       accid$C_HOUR,'_',accid$C_RCFG,accid$C_WTHR,
                       accid$C_RSUR,accid$C_RALN,accid$C_TRAF,'_',
                       accid$C_CONF,accid$C_VEHS,accid$C_SEV,
                       sep='')

accid$vehicID <- paste(accid$occurID,'_',accid$V_ID,
                       accid$V_TYPE,accid$V_YEAR,
                       sep='')

# Save Count of vehIDs within occID to numVehs
accid$numVehs <- as.character(ave(accid$vehicID,accid$occurID,FUN=function(x)length(unique(x))))
accid$numVehs <- ifelse(nchar(accid$numVehs)<2,paste('0',accid$numVehs,sep=''),accid$numVehs)

# Original dataset had count of vehicles in accident. Check if above step yielded same
# vehicle counts.
nrow(subset(accid,accid$numVehs!=accid$C_VEHS)) #/nrow(accid)
# 2MM records did not match, out of 5.8MM -  34%

accid$persnID <- paste(accid$vehicID,'_',accid$P_ID,
                       accid$P_PSN,accid$P_ISEV,
                       accid$P_SAFE,accid$P_USER,
                       sep='')

# MISSING if any of above ID contains U|X, --OR-- Age of driver unknown
# Applicable to Occurences - will remove Occurence if any are TRUE

vehType <- c('01','05','06')    #Private passenger, vans, light trucks
accid$exclude <- (regexpr('U|X',accid$persnID)>0)|                   #Missing time, road condition, injury info, etc
                 (accid$P_USER=='1' & accid$P_AGE %in% missingVal)|  #Missing Age of Driver
                 (accid$numVehs!=accid$C_VEHS)|                      #Original Veh COunt doesn't match unique num of IDs  
                 !(accid$V_TYPE %in% vehType)                        #Vehicle is not priv passenger, van or light truck

# To Remove entire Occurences if it contains at least 1 record with
# missing key info ($exclude=TRUE)
# 
occurToRemove <- unique(accid$occurID[which(accid$exclude==TRUE)])

# Quick Metrics
# 
length(unique(accid$occurID[which(accid$exclude==TRUE)]))/length(unique(accid$occurID))
# 61% of occurrence IDs will be excluded
nrow(subset(accid,accid$occurID %in% occurToRemove))/nrow(accid)
# 67% of all records will be removed

# REMOVE excluded
# 
accid.cln <- accid[which(!(accid$occurID %in% occurToRemove)),]
nrow(accid.cln)
length(unique(accid.cln$occurID))
# 1.9MM records
# 721K occurences, spread reasonably even throughout years - as per below
for (i in 1999:2014){print(length(unique(accid.cln$occurID[which(accid.cln$C_YEAR==i)])))}

## Another Data Summary Table for cleaned data
##
defitn.tbl.cln <- defitn.tbl
for (i in 1:22){defitn.tbl.cln$values[i] <- unique(accid.cln[i])
                defitn.tbl.cln$numNA[i] <- sum(accid.cln[,i] %in% missingVal)
}
View(defitn.tbl.cln)


###########################################################################
#####################  Section 3: DATA ANALYSIS  ##########################
###########################################################################

# remove C_SEV, we have P_ISEV
# remove P_PSN, we have P_USER
# remove gender
# remove numVehs, exclude
# modify P_SAFE to restrictive, preventative, none, other


# Example: dataset <- dataset[,c(1:2, 6, 3:5)]


######  SAND BOX  ######

nrow(accid[which(accid$V_ID=='UU'|accid$P_ID=='UU'),])
# not many with unknown veh/person sequence number
# 436 people records with unknown unknown veh/person
# sequence number

length(unique(accid[which(accid$V_ID=='UU'|accid$P_ID=='UU'),]$occurID))
# 256 accident occurence ID's associated with unknown veh/person
# sequence number

# Remove accid records with unknown veh/person sequence number
unknownOccur <- unique(accid[which(accid$V_ID=='UU'|accid$P_ID=='UU'),]$occurID)
accid.new <- accid[which(!(accid$occurID %in% unknownOccur)),]
# check
nrow(accid.new[which(accid.new$V_ID=='UU'|accid.new$P_ID=='UU'),])

# Exploring

# x11()
# hist(as.numeric(accid$P_AGE))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='1999')))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='2004')))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='2009')))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='2014')))

x11()
boxplot(as.numeric(accid.indiv$P_AGE)~accid.indiv$C_YEAR,accid.indiv)
# lower whisker always at 0??? remove if outlier - children vehicle occupants
summary(as.numeric(accid.indiv$P_AGE))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     1.0    22.0    34.0    36.4    49.0    99.0  395156 

#Check if User consistent with Position. ex, All User=Driver, Position=Driver (as oppose to 
#front or rear Passenger)
#
sort(unique(subset(accid.indiv,accid.indiv$P_USER=='1')$P_PSN)) #Good
sort(unique(subset(accid.indiv,accid.indiv$P_USER=='2')$P_PSN)) #Good
sort(unique(subset(accid.indiv,accid.indiv$P_USER=='3')$P_PSN)) #Suspect
sort(unique(subset(accid.indiv,accid.indiv$P_USER=='4')$P_PSN)) #Suspect
sort(unique(subset(accid.indiv,accid.indiv$P_USER=='5')$P_PSN)) #Suspect
sort(unique(subset(accid.indiv,accid.indiv$P_USER=='U')$P_PSN)) #Suspect

                                  
                                  
                                  
###############################################################################
### OLD ### OLD ### OLD ### OLD ### OLD ### OLD ### OLD ### OLD ### OLD ### OLD 
###############################################################################
                                  
sum(is.na(accid$P_ISEV))
summary(accid$P_ISEV)$coefficients
?summary

accid.sev <- accid$P_ISEV

as.numeric(table(accid.sev)[5])/length(accid.sev)

# Export to Excel for quick look
test <- accid[which(accid$V_TYPE=='01'
                    &accid$C_MNTH=='02' #feb
                    #&accid$C_YEAR %in% seq(1999,2001,by=1)
                    &accid$C_YEAR==2001
                    &accid$C_VEHS %in% c('01','02','03')
                    ),]

test$C_ID <- paste(test$C_YEAR
                   ,test$C_MNTH
                   ,test$C_WDAY
                   ,test$C_HOUR
                   ,test$C_CONF
                  ,test$C_RCFG
                  ,test$C_WTHR
                  ,test$C_RSUR
                  ,test$C_RALN
                  ,test$C_TRAF
                  ,sep='')

# remove incidents where # cars not equal # drivers - could have been identical seperate collisions
# or involving pedestrians, bikes, which I removed the other parties.
# ONLY LOOKING AT VEHICLE/VEHICLE incidents
str(test)

test$C_DRVS <- 0
IncidIndex <- unique(test$C_ID)
for(i in 1:length(IncidIndex)){#build number of drivers in the incident
  test$C_DRVS[which(test$C_ID==IncidIndex[i])] <- 
    nrow(test[which(test$P_USER=='1'&test$C_ID==IncidIndex[i]),])
  #re-evaluate C_SEV=2 (vs 1), break them out to no inj (P_ISEV=1) to non-fatal inj (P_ISEV=2)
  #DONE IN EXCEL
}

str(test$C_SEV)
str(test$P_ISEV)
test <- test[which(test$C_DRVS==as.numeric(test$C_VEHS)),]

write.csv(test,"test_accidents.csv",row.names = FALSE)
rm(test)

test$C_DRVS[which(test$C_ID==IncidIndex[4000])]


myaccid.2014 <- accid[which(accid$V_TYPE=='01'
                      #&accid$C_MNTH=='02' #feb
                      #&accid$C_YEAR %in% seq(1999,2014,by=5)
                      &accid$C_YEAR==2014
                      &accid$C_VEHS %in% c('01','02','03')
                      ),]

myaccid.2014$C_ID <- paste(myaccid.2014$C_YEAR
                      ,myaccid.2014$C_MNTH
                      ,myaccid.2014$C_WDAY
                      ,myaccid.2014$C_HOUR
                      ,"_"
                    ,myaccid.2014$C_CONF
                    ,myaccid.2014$C_RCFG
                    ,myaccid.2014$C_WTHR
                    ,myaccid.2014$C_RSUR
                    ,myaccid.2014$C_RALN
                    ,myaccid.2014$C_TRAF
                    ,sep='')

# remove incidents where # cars not equal # drivers - could have been identical seperate collisions
# or involving pedestrians, bikes, which I removed the other parties.
# ONLY LOOKING AT VEHICLE/VEHICLE incidents

myaccid.1999$C_DRVS <- 0
IncidIndex <- unique(myaccid.1999$C_ID)
for(i in 1:length(IncidIndex)){
  myaccid.1999$C_DRVS[which(myaccid.1999$C_ID==IncidIndex[i])] <- 
    nrow(myaccid.1999[which(myaccid.1999$P_USER=='1'&myaccid.1999$C_ID==IncidIndex[i]),])
}

myaccid.2014 <- myaccid.2014[which(myaccid.2014$C_DRVS==as.numeric(myaccid.2014$C_VEHS)),]

str(myaccid)
table(myaccid$C_YEAR)

write.csv(myaccid.1999,"accidents_1999.csv",row.names = FALSE)
write.csv(myaccid.2004,"accidents_2004.csv",row.names = FALSE)
write.csv(myaccid.2009,"accidents_2009.csv",row.names = FALSE)
write.csv(myaccid.2014,"accidents_2014.csv",row.names = FALSE)

table(myaccid$V_TYPE)
table(accid$C_VEHS)[4]/5860405
head(myaccid)



#############################################################
###########################STAGE 2###########################
#############################################################
###COUNT NUMBER OF ACCIDENTS PER AGE GROUP############



accid2.1999 <- read.csv("F:/Marsh/accidents_1999_preStage2.csv"
                  ,header = TRUE,stringsAsFactors = FALSE)
accid2.2004 <- read.csv("F:/Marsh/accidents_2004_preStage2.csv"
                  ,header = TRUE,stringsAsFactors = FALSE)
accid2.2009 <- read.csv("F:/Marsh/accidents_2009_preStage2.csv"
                  ,header = TRUE,stringsAsFactors = FALSE)
accid2.2014 <- read.csv("F:/Marsh/accidents_2014_preStage2.csv"
                  ,header = TRUE,stringsAsFactors = FALSE)

names(accid2.1999)[1] <- paste("C_ID")
names(accid2.2004)[1] <- paste("C_ID")
names(accid2.2009)[1] <- paste("C_ID")
names(accid2.2014)[1] <- paste("C_ID")

ageGrp <- c("Under 17","17-26","26-40","41-60","61-99")

summ.1999 <- data.frame()
for(i in 1:length(unique(accid2.1999$C_SEV_g))){
  for(j in 1:length(ageGrp)){
    summ.1999[i,j] <- length(unique(accid2.1999$C_ID[which(accid2.1999$C_SEV_g==i
                                                           &accid2.1999$C_MINA==ageGrp[j])]))
  }
}

summ.2004 <- data.frame()
for(i in 1:length(unique(accid2.2004$C_SEV_g))){
  for(j in 1:length(ageGrp)){
    summ.2004[i,j] <- length(unique(accid2.2004$C_ID[which(accid2.2004$C_SEV_g==i
                                                           &accid2.2004$C_MINA==ageGrp[j])]))
  }
}

summ.2009 <- data.frame()
for(i in 1:length(unique(accid2.2009$C_SEV_g))){
  for(j in 1:length(ageGrp)){
    summ.2009[i,j] <- length(unique(accid2.2009$C_ID[which(accid2.2009$C_SEV_g==i
                                                           &accid2.2009$C_MINA==ageGrp[j])]))
  }
}

summ.2014 <- data.frame()
for(i in 1:length(unique(accid2.2014$C_SEV_g))){
  for(j in 1:length(ageGrp)){
    summ.2014[i,j] <- length(unique(accid2.2014$C_ID[which(accid2.2014$C_SEV_g==i
                                                           &accid2.2014$C_MINA==ageGrp[j])]))
  }
}

write.csv(summ.1999,"accidents_1999_postStage2.csv")
write.csv(summ.2004,"accidents_2004_postStage2.csv")
write.csv(summ.2009,"accidents_2009_postStage2.csv")
write.csv(summ.2014,"accidents_2014_postStage2.csv")

View(head(summ.1999))
# select every 5 years
# scope <- seq(1999,2014,by=5)
# myaccid <- accid[which(accid$C_YEAR==1999),]
# for(i in 2:length(scope)){
#   myaccid <- rbind(myaccid,accid[which(accid$C_YEAR==scope[i]),],header=FALSE)
# }
