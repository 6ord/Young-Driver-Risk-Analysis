rm(list=ls())

# install.packages("sparklyr")
# library(sparklyr)

# install.packages("data.table")
# library(data.table)
# Tried data.table, no noticable difference in run times. Code is simpler though.

# setwd('F:\\Ryerson\\CKME136_Capstone\\repository')
setwd('C:\\Users\\trunk\\Downloads\\BigData\\repository')

source('accid_base.r') #packages, importing, build occur, veh and person IDs
source('accid_range.r') #build bands/ranges for age, time of day, time of week, rd/crash config, etc

#Check 100 random records
#View(accid.cln[sample(1:nrow(accid.cln),100),])

# SKip to line 100

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
defitn.tbl$attrType <- c('Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Quan-Discrete','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Ordinal','Qual-Nominal','Qual-Ordinal','Qual-Ordinal','Qual-Nominal','Qual-Ordinal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal')

for (i in 1:22){defitn.tbl$values[i] <- unique(accid[i])
defitn.tbl$numNA[i] <- sum(accid[,i] %in% missingVal)
}

# Check how many drivers are missing age
sum(subset(accid,accid$P_USER=='1')$P_AGE %in% missingVal)
# 150K+

#View(defitn.tbl)

###########################################################################
#####################  Section 2: DATA CLEANING  ##########################
###########################################################################
# Original dataset had count of vehicles in accident. Check if above step yielded same
# vehicle counts.
nrow(subset(accid,accid$numVehs!=accid$C_VEHS)) #/nrow(accid)
# 2MM records did not match, out of 5.8MM -  34%

# Quick Metrics
# 
length(unique(accid$occurID[which(accid$exclude==TRUE)]))/length(unique(accid$occurID))
# 61% of occurrence IDs will be excluded
nrow(subset(accid,accid$occurID %in% occurToRemove))/nrow(accid)
# 67% of all records will be removed

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

## Another Data Summary Table for cleaned data
##
rm(defitn.tbl,defitn.tbl.cln,dataDictionary,i)

###########################################################################
###################  Section 3: PRELIM DATA TRENDS  #######################
###########################################################################

#Important for Association Rules Section around line 380
expl.fields.r <- colnames(accid.cln[28:33])
asso.fields.r <- c(expl.fields.r,colnames(accid.cln[20]))

# Passenger / Driver Linkage
# Building DRV_AGE:
# - Build temporary dataframe of unique vehicle IDs and its driver's age ('accid.drvs'),
#   remove vehicle IDs where there were multiple drivers (remove data anomalies), then
#   lastly, merge accid.drvs with original cleaned dataset.
#   
driver.fields <- c(colnames(accid.cln[1]),colnames(accid.cln[22]),colnames(accid.cln[24]),
                   colnames(accid.cln[18]),expl.fields.r,colnames(accid.cln[20]))
accid.drvs <- accid.cln[accid.cln$P_USER=='1'& accid.cln$P_AGE!='NN',] #filtered out 980,667 records (to 1,737,594)
# > nrow(subset(accid.cln,accid.cln$P_USER!='1'))
# [1] 980652
# > nrow(subset(accid.cln,accid.cln$P_USER=='1'&accid.cln$P_AGE=='NN'))
# [1] 15
# 980,652 drivers, 15 of them did not have age captured.
# 
accid.drvs <- accid.drvs[driver.fields]
accid.drvs$P_AGE <-as.numeric(accid.drvs$P_AGE) 
#Expect as many drivers as vehicle IDs (one driver per vehicle)
#Remove vehicle IDs where there's more than 1 driver. In other words,
#remove drivers with common vehicleID
tempTab <- table(accid.drvs$vehicID)
accid.drvs <- accid.drvs[which(!accid.drvs$vehicID %in% data.frame(tempTab[tempTab>1])[,1]),]
nrow(accid.drvs)-1737594 #from line 336

rm(tempTab)

#Check
nrow(accid.drvs)==length(unique(accid.drvs$vehicID))
nrow(accid.drvs)==sum(as.numeric(accid.drvs$P_USER))

accid.cln.wDrvAge <- merge(accid.drvs[c('vehicID','P_AGE','P_AGE_r')],accid.cln,by='vehicID', all.y=TRUE)
# > length(unique(accid.cln$vehicID))
# [1] 1853318
# > length(unique(accid.cln.wDrvAge$vehicID))
# [1] 1853318
# 
# Tidy column names
newFields <- colnames(accid.cln.wDrvAge)
newFields[2:3] <- c('DRV_AGE','DRV_AGE_r')
newFields[21] <- c('P_AGE')
newFields[30] <- c('P_AGE_r')
colnames(accid.cln.wDrvAge) <- newFields

# Left Join from line 356 yielded 202,111 of 2.7MM records with NULL
# DRV_AGE. (There was no driver age found)
# 
# > sum(is.na(accid.cln.wDrvAge$DRV_AGE))
# [1] 202111
# > sum(!is.na(accid.cln.wDrvAge$DRV_AGE))
# [1] 2516150
# 
# Removing them:
accid.cln.wDrvAge <- accid.cln.wDrvAge[which(!is.na(accid.cln.wDrvAge$DRV_AGE)),]

# Test Review some records
rm(driver.fields,accid.drvs
   #,accid.cln
   )
length(unique(accid.cln.wDrvAge$vehicID))
#View(accid.cln.wDrvAge[sample(1:nrow(accid.cln.wDrvAge),100),])

# Randomly take 10 sequential Vehicle ID records to examine. Ensure driver age
# was captured on passenger records.
uniqueVehIDs <- unique(accid.cln.wDrvAge$vehicID)
raNum <- sample(11:length(uniqueVehIDs)-11,1)
sampleVehID <- uniqueVehIDs[(raNum-10):raNum]
rm(uniqueVehIDs,raNum)
#View(sampleVehID) #Random 10 consequtive Vehicle IDs
View(subset(accid.cln.wDrvAge,accid.cln.wDrvAge$vehicID==sampleVehID[2])[c('vehicID','P_USER',
                                                                           'P_AGE','DRV_AGE',
                                                                           'DRV_AGE_r')])

###################################
####### Frequency Plots ###########
####### -By Injury Line 207 #######
####### -By driverAge Line 379 ####
###################################

#Weather and Rd Surface redundent? (C_WTHR vs C_RSUR)
#
rdVsWthr <- as.data.frame(cbind(road=accid.cln$C_RSUR,
                                wthr=accid.cln$C_WTHR))
#group some roads so have same length as wther
rdVsWthr[which(rdVsWthr$road %in% c('7','8')),]$road <- '6'
rdVsWthr$road <- factor(rdVsWthr$road,
                        levels=c('1','2','3',
                                 '4','5','6',
                                 '9','Q'))
chisq.test(rdVsWthr$road,rdVsWthr$wthr,correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  rdVsWthr$road and rdVsWthr$wthr
# X-squared = 2865700, df = 49, p-value <
# 2.2e-16
# 
# small p-value, fail to reject H0 that rd and wthr
# are independent. They are dependent of one another.
# Manual review below reveal anything goes. Road can
# be dry when it's raining or snowing! Only trend is
# it's not snowing or icy when road is flooded.
unique(subset(rdVsWthr,rdVsWthr$road=='9')$wthr)
rm(rdVsWthr)

##############################################
####### Frequency Plots: All by Injury #######
##############################################
#
#(old)aggregate(persnID~P_ISEV+C_YEAR, data=accid.cln, length)
#(old)aggregate(persnID~C_YEAR+C_WDAY_r, data=subset(accid.cln,accid.cln$P_ISEV=='2'), length)

#P_ISEV Comparison
x11()
par(mfrow=c(1,2))
barplot(table(accid.cln$P_ISEV,accid.cln$C_YEAR),
        main='Counts of Not Injured (1), Injured (2) and Fatal (3)',
        xlab='P_ISEV',
        ylab='Frequency',
        col=rainbow(3),
        legend=rownames(table(accid.cln$P_ISEV,accid.cln$C_YEAR)),
        beside=TRUE)
# Deep Dive in Fatal only
# x11()
barplot(table(x=factor(accid.cln$P_ISEV,exclude=c('1','2')),accid.cln$C_YEAR),
        main='Counts of Fatal (3)',
        xlab='P_ISEV=3',
        ylab='Frequency',
        col='blue'
        #legend=rownames(table(accid.cln$P_ISEV,accid.cln$C_YEAR)),
        #beside=TRUE
)

#Create Subsets by Injury Level
#
accid.cln.fatal <- subset(accid.cln,accid.cln$P_ISEV=='3')
accid.cln.injrd <- subset(accid.cln,accid.cln$P_ISEV=='2')
accid.cln.noinj <- subset(accid.cln,accid.cln$P_ISEV=='1')
# Check
nrow(accid.cln)==sum(nrow(accid.cln.fatal),
                     nrow(accid.cln.injrd),
                     nrow(accid.cln.noinj)
)

# Fatal Injuries by Time
x11()
par(mfrow=c(1,3))
barplot(table(accid.cln.fatal$C_HOUR_r,accid.cln.fatal$C_WDAY_r),
        main='Fatal Injuries by Time',
        xlab='Fatal',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.fatal$C_HOUR_r))),
        #legend=rownames(table(accid.cln.fatal$C_HOUR_r,accid.cln.fatal$C_WDAY_r)),
        beside=TRUE)
# Injuries by Time
# x11()
barplot(table(accid.cln.injrd$C_HOUR_r,accid.cln.injrd$C_WDAY_r),
        main='Injuries by Time',
        xlab='Injured',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.injrd$C_HOUR_r))),
        legend=rownames(table(accid.cln.injrd$C_HOUR_r,accid.cln.injrd$C_WDAY_r)),
        beside=TRUE)
barplot(table(accid.cln.noinj$C_HOUR_r,accid.cln.noinj$C_WDAY_r),
        main='No Injuries by Time',
        xlab='Not Injured',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.noinj$C_HOUR_r))),
        legend=rownames(table(accid.cln.noinj$C_HOUR_r,accid.cln.noinj$C_WDAY_r)),
        beside=TRUE)

#What if we count Friday night as weekend, and Sunday night as weekday??
#
fields.friday.r <- c(colnames(accid.cln[3]),expl.fields.r,colnames(accid.cln[20]))
temp.accid <- accid.cln[fields.friday.r]

temp.accid.fatal <- subset(accid.cln,accid.cln$P_ISEV=='3')
temp.accid.injrd <- subset(accid.cln,accid.cln$P_ISEV=='2')
temp.accid.noinj <- subset(accid.cln,accid.cln$P_ISEV=='1')

# Fatal Injuries
x11()
barplot(table(temp.accid.fatal$C_HOUR_r,temp.accid.fatal$C_WDAY),
        main='Fatal Injuries by Day (Mon-Sun)',
        xlab='Fatal',
        ylab='Frequency',
        col=rainbow(length(unique(temp.accid.fatal$C_HOUR_r))),
        legend=rownames(table(temp.accid.fatal$C_HOUR_r,temp.accid.fatal$C_WDAY)),
        beside=TRUE)
# Injuries
x11()
barplot(table(temp.accid.injrd$C_HOUR_r,temp.accid.injrd$C_WDAY),
        main='Injuries by Day (Mon-Sun)',
        xlab='Injured',
        ylab='Frequency',
        col=rainbow(length(unique(temp.accid.injrd$C_HOUR_r))),
        legend=rownames(table(temp.accid.injrd$C_HOUR_r,temp.accid.injrd$C_WDAY)),
        beside=TRUE)
#Friday night/overnight indeed has a lot. Surprisingly, Sunday overnight was high too.
rm(temp.accid,temp.accid.fatal,temp.accid.injrd,temp.accid.noinj, fields.friday.r)

# Fatal Injuries by Rd Config
x11()
par(mfrow=c(1,3))
barplot(table(accid.cln.fatal$C_RCFG_r,accid.cln.fatal$C_YEAR),
        main='Fatal Injuries by Rd Config',
        xlab='Fatal',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.fatal$C_RCFG_r))),
        col=rainbow(7),
        legend=rownames(table(accid.cln.fatal$C_RCFG_r,accid.cln.fatal$C_YEAR)),
        beside=TRUE)
# Injuries by Rd Config
# x11()
barplot(table(accid.cln.injrd$C_RCFG_r,accid.cln.injrd$C_YEAR),
        main='Injuries by Rd Config',
        xlab='Injured',
        ylab='Frequency',
        col=rainbow(7),
        legend=rownames(table(accid.cln.injrd$C_RCFG_r,accid.cln.injrd$C_YEAR)),
        beside=TRUE)
barplot(table(accid.cln.noinj$C_RCFG_r,accid.cln.noinj$C_YEAR),
        main='No Injuries by Rd Config',
        xlab='Not Injured',
        ylab='Frequency',
        col=rainbow(7),
        legend=rownames(table(accid.cln.noinj$C_RCFG_r,accid.cln.noinj$C_YEAR)),
        beside=TRUE)


# Fatal Injuries by Traffic Control
x11()
par(mfrow=c(1,3))
barplot(table(accid.cln.fatal$C_TRAF_r,accid.cln.fatal$C_YEAR),
        main='Fatal Injuries by Traffic Control',
        xlab='Fatal',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.fatal$C_TRAF_r))),
        legend=rownames(table(accid.cln.fatal$C_TRAF_r,accid.cln.fatal$C_YEAR)),
        beside=TRUE)
# Injuries by Traffic Control
# x11()
barplot(table(accid.cln.injrd$C_TRAF_r,accid.cln.injrd$C_YEAR),
        main='Injuries by Traffic Control',
        xlab='Injured',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.injrd$C_TRAF_r))),
        legend=rownames(table(accid.cln.injrd$C_TRAF_r,accid.cln.injrd$C_YEAR)),
        beside=TRUE)
barplot(table(accid.cln.noinj$C_TRAF_r,accid.cln.noinj$C_YEAR),
        main='No Injuries by Traffic Control',
        xlab='Not Injured',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.noinj$C_TRAF_r))),
        # legend=rownames(table(accid.cln.injrd$C_TRAF_r,accid.cln.injrd$C_YEAR)),
        beside=TRUE)

# Fatal Injuries by Collision Config
x11()
par(mfrow=c(1,3))
barplot(table(accid.cln.fatal$C_CONF_r,accid.cln.fatal$C_YEAR),
        main='Fatal Injuries by Collision Config',
        xlab='Fatal',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.fatal$C_CONF_r))),
        legend=rownames(table(accid.cln.fatal$C_CONF_r,accid.cln.fatal$C_YEAR)),
        beside=TRUE)
# Injuries by Collision Config
# x11()
barplot(table(accid.cln.injrd$C_CONF_r,accid.cln.injrd$C_YEAR),
        main='Injuries by Collision Config',
        xlab='Injured',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.injrd$C_CONF_r))),
        legend=rownames(table(accid.cln.injrd$C_CONF_r,accid.cln.injrd$C_YEAR)),
        beside=TRUE)
barplot(table(accid.cln.noinj$C_CONF_r,accid.cln.noinj$C_YEAR),
        main='No Injuries by Collision Config',
        xlab='Not Injured',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.noinj$C_CONF_r))),
        legend=rownames(table(accid.cln.noinj$C_CONF_r,accid.cln.noinj$C_YEAR)),
        beside=TRUE)

#Why are there so many single car collision records in 1999???
#
accid.occur.fields <- c(colnames(accid.cln[1]),colnames(accid.cln[23]),expl.fields.r,colnames(accid.cln[20]))
temp.accid <- accid.cln[accid.occur.fields]
temp.accid.uniq <- temp.accid[which(!duplicated(temp.accid$occurID)),]
rm(accid.occur.fields,temp.accid)
#Check num records
length(unique(accid.cln$occurID))==nrow(temp.accid.uniq)

x11()
barplot(table(temp.accid.uniq$C_CONF_r,temp.accid.uniq$C_YEAR),
        main='Collisions by Collision Config',
        xlab='All Collisions',
        ylab='Frequency',
        col=rainbow(length(unique(temp.accid.uniq$C_CONF_r))),
        legend=rownames(table(temp.accid.uniq$C_CONF_r,temp.accid.uniq$C_YEAR)),
        beside=TRUE)
#Still high in 1999. But noticed Single Other was very low - suspect recoding from
#Single Other to Single Collision after 1999
rm(temp.accid.uniq)

rm(accid.cln.fatal,
   accid.cln.injrd,
   accid.cln.noinj
  )

#################################################
####### Frequency Plots: All by DriverAge #######
#################################################

#High Level DrvAge Comparison
zoom17To49 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE %bw% c(17,49))

x11()
par(mfrow=c(2,1))
barplot(table(accid.cln.wDrvAge$DRV_AGE_r,accid.cln.wDrvAge$C_YEAR),
        main='Counts of Colliding Vehicle Occupants',
        xlab='Driver Age',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.wDrvAge$DRV_AGE_r))),
        col=rainbow(6),
        legend=rownames(table(accid.cln.wDrvAge$DRV_AGE_r,accid.cln.wDrvAge$C_YEAR)),
        beside=TRUE)
# Zoom in between 17-49
# zoom17To49 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE %bw% c(17,49))
# x11()
barplot(table(zoom17To49$DRV_AGE,zoom17To49$C_YEAR),
        main='Counts of Colliding Vehicle Occupants',
        xlab='DRV_AGE (17-49)',
        ylab='Frequency',
        col='blue',
        #legend=rownames(table(accid.cln.wDrvAge$DRV_AGE_r,accid.cln.wDrvAge$C_YEAR)),
        border=NA,
        beside=TRUE)

rm(zoom17To49)

#Create Subsets by Age Group
#
accid.cln.17 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE_r=='17-25')
accid.cln.26 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE_r=='26-35')
accid.cln.36 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE_r=='36-49')

# Occupants by 3 top age groups by Time
x11()
par(mfrow=c(3,1))
barplot(table(accid.cln.17$C_HOUR_r,accid.cln.17$C_WDAY_r),
        main='Occupants by Time',
        xlab='Drv Age 17-25',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.17$C_HOUR_r))),
        legend=rownames(table(accid.cln.17$C_HOUR_r,accid.cln.17$C_WDAY_r)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.26$C_HOUR_r,accid.cln.26$C_WDAY_r),
        main='Occupants by Time',
        xlab='Drv Age 26-35',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.26$C_HOUR_r))),
        #legend=rownames(table(accid.cln.26$C_HOUR_r,accid.cln.26$C_WDAY_r)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.36$C_HOUR_r,accid.cln.36$C_WDAY_r),
        main='Occupants by Time',
        xlab='Drv Age 36-49',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.36$C_HOUR_r))),
        #legend=rownames(table(accid.cln.36$C_HOUR_r,accid.cln.36$C_WDAY_r)),
        beside=TRUE)
# 
# #What if we count Friday night as weekend, and Sunday night as weekday??
# #
# fields.friday.r <- c(colnames(accid.cln[3]),expl.fields.r,colnames(accid.cln[20]))
# temp.accid <- accid.cln[fields.friday.r]
# 
# temp.accid.fatal <- subset(accid.cln,accid.cln$P_ISEV=='3')
# temp.accid.injrd <- subset(accid.cln,accid.cln$P_ISEV=='2')
# temp.accid.noinj <- subset(accid.cln,accid.cln$P_ISEV=='1')
# 
# # Fatal Injuries
# x11()
# barplot(table(temp.accid.fatal$C_HOUR_r,temp.accid.fatal$C_WDAY),
#         main='Fatal Injuries by Day (Mon-Sun)',
#         xlab='Fatal',
#         ylab='Frequency',
#         col=rainbow(length(unique(temp.accid.fatal$C_HOUR_r))),
#         legend=rownames(table(temp.accid.fatal$C_HOUR_r,temp.accid.fatal$C_WDAY)),
#         beside=TRUE)
# # Injuries
# x11()
# barplot(table(temp.accid.injrd$C_HOUR_r,temp.accid.injrd$C_WDAY),
#         main='Injuries by Day (Mon-Sun)',
#         xlab='Injured',
#         ylab='Frequency',
#         col=rainbow(length(unique(temp.accid.injrd$C_HOUR_r))),
#         legend=rownames(table(temp.accid.injrd$C_HOUR_r,temp.accid.injrd$C_WDAY)),
#         beside=TRUE)
# #Friday night/overnight indeed has a lot. Surprisingly, Sunday overnight was high too.
# rm(temp.accid,temp.accid.fatal,temp.accid.injrd,temp.accid.noinj, fields.friday.r)

# Top 3 Age Groups by Rd Config
x11()
par(mfrow=c(3,1))
barplot(table(accid.cln.17$C_RCFG_r,accid.cln.17$C_YEAR),
        main='Occupants by Rd Config',
        xlab='Drv Age 17-25',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.17$C_RCFG_r))),
        col=rainbow(6),
        legend=rownames(table(accid.cln.17$C_RCFG_r,accid.cln.17$C_YEAR)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.26$C_RCFG_r,accid.cln.26$C_YEAR),
        main='Occupants by Rd Config',
        xlab='Drv Age 26-35',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.26$C_RCFG_r))),
        col=rainbow(6),
        #legend=rownames(table(accid.cln.26$C_RCFG_r,accid.cln.26$C_YEAR)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.36$C_RCFG_r,accid.cln.36$C_YEAR),
        main='Occupants by Rd Config',
        xlab='Drv Age 36-49',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.36$C_RCFG_r))),
        col=rainbow(6),
        #legend=rownames(table(accid.cln.36$C_RCFG_r,accid.cln.36$C_YEAR)),
        beside=TRUE)


# Top 3 Age Groups by Traffic Control
x11()
par(mfrow=c(3,1))
barplot(table(accid.cln.17$C_TRAF_r,accid.cln.17$C_YEAR),
        main='Occupants by Traffic Control',
        xlab='Drv Age 17-25',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.17$C_TRAF_r))),
        col=rainbow(8),
        legend=rownames(table(accid.cln.17$C_TRAF_r,accid.cln.17$C_YEAR)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.26$C_TRAF_r,accid.cln.26$C_YEAR),
        main='Occupants by Traffic Control',
        xlab='Drv Age 26-35',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.26$C_TRAF_r))),
        col=rainbow(8),
        #legend=rownames(table(accid.cln.26$C_TRAF_r,accid.cln.26$C_YEAR)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.36$C_TRAF_r,accid.cln.36$C_YEAR),
        main='Occupants by Traffic Control',
        xlab='Drv Age 36-49',
        ylab='Frequency',
        #col=rainbow(length(unique(accid.cln.36$C_TRAF_r))),
        col=rainbow(8),
        #legend=rownames(table(accid.cln.36$C_TRAF_r,accid.cln.36$C_YEAR)),
        beside=TRUE)


# Top 3 Age Groups by Collision Configuration
x11()
par(mfrow=c(3,1))
barplot(table(accid.cln.17$C_CONF_r,accid.cln.17$C_YEAR),
        main='Occupants by Collision Config',
        xlab='Drv Age 17-25',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.17$C_CONF_r))),
        #col=rainbow(8),
        legend=rownames(table(accid.cln.17$C_CONF_r,accid.cln.17$C_YEAR)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.26$C_CONF_r,accid.cln.26$C_YEAR),
        main='Occupants by Collision Config',
        xlab='Drv Age 26-35',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.26$C_CONF_r))),
        #col=rainbow(8),
        #legend=rownames(table(accid.cln.26$C_CONF_r,accid.cln.26$C_YEAR)),
        beside=TRUE)
# x11()
barplot(table(accid.cln.36$C_CONF_r,accid.cln.36$C_YEAR),
        main='Occupants by Collision Config',
        xlab='Drv Age 36-49',
        ylab='Frequency',
        col=rainbow(length(unique(accid.cln.36$C_CONF_r))),
        #col=rainbow(8),
        #legend=rownames(table(accid.cln.36$C_CONF_r,accid.cln.36$C_YEAR)),
        beside=TRUE)



##### Exploring Association Rules
#####

fat_rules <- sort(apriori(accid.cln[asso.fields.r],
                          parameter=list(supp=0.1,conf=0.2,minlen=3),
                          appearance=list(rhs='P_ISEV=3')),
                  by='lift')

inj_rules <- sort(apriori(accid.cln[asso.fields.r],
                          parameter=list(supp=0.1,conf=0.2,minlen=3),
                          appearance=list(rhs='P_ISEV=2')),
                  by='lift')

noinj_rules <- sort(apriori(accid.cln[asso.fields.r],
                            parameter=list(supp=0.1,conf=0.2,minlen=3),
                            appearance=list(rhs='P_ISEV=1')),
                    by='lift')

inspect(fat_rules)
inspect(inj_rules)
inspect(noinj_rules)
# cleanup
rm(fat_rules,inj_rules,noinj_rules)


# Decision Tree
# https://www.youtube.com/watch?v=DYvFapXJoZY


#Stacked Bar: P_ISEV Freq by year
x11()
ggplot() + geom_bar(aes(y = persnID, x = C_YEAR, fill = P_ISEV),
                        data = aggregate(persnID~C_YEAR+P_ISEV, data=accid.cln, FUN=length),
                        stat="identity") + coord_flip()

#Stacked Bar: time of day Freq by year+P_ISEV
x11()
ggplot() + geom_bar(aes(y = persnID, x = C_YEAR, fill = C_HOUR_r),
                    data = aggregate(persnID~C_YEAR+C_HOUR_r, data=subset(accid.cln,accid.cln$P_ISEV=='3'), FUN=length),
                    stat="identity") + coord_flip()

# [OLD, changed accid$exclude build after.]Check number of records/people with P_ISEV=N,
# in which collision configs and number of vehicles. Two highest counts 30K+ 
# were in clear two vehicle collisions. Question this field.
# 
aggregate(P_ID~C_CONF_r, data=subset(accid.cln,accid.cln$P_ISEV=='N'), FUN=length)
#        C_CONF_r C_VEHS  P_ID
#        ...
# 7    two_OneDir     02 31126
# 8  two_multiDir     02 37222
#        ...

# SUBSET OUT DRIVERS

NAFdrvs <- accid.cln[accid.cln$P_USER=='1'&
                       accid.cln$P_AGE!='NN'&
                       as.numeric(accid.cln$V_ID)>1,]

AFdrvs <- accid.cln[accid.cln$P_USER=='1'&
                      accid.cln$P_AGE!='NN'&
                      as.numeric(accid.cln$V_ID)==1,]


x11()       
boxplot(as.numeric(NAFdrvs$P_AGE)~NAFdrvs$C_YEAR
        ,NAFdrvs
        ,horizontal=TRUE
        ,main='Age Distribution of NAF Drivers'
)

x11()       
boxplot(as.numeric(AFdrvs$P_AGE)~AFdrvs$C_YEAR
        ,AFdrvs
        ,horizontal=TRUE
        ,main='Age Distribution of AF Drivers'
)

# Summary of AF and NAF driver age
summary(as.numeric(AFdrvs$P_AGE))
summary(as.numeric(NAFdrvs$P_AGE))

#Check
#
unique(NAFdrv.boxplot.data[NAFdrv.boxplot.data$C_HOUR_r=='overnight',]$C_HOUR)
unique(NAFdrv.boxplot.data[NAFdrv.boxplot.data$C_HOUR_r=='PM_late',]$C_HOUR)
# OK

#Summary: number of accidents for age group, time of day
NAF_agg <- aggregate(occurID~C_HOUR_r+P_AGE_r,NAFdrv.boxplot.data,FUN=function(x){length(unique(x))})
AF_agg <- aggregate(occurID~C_HOUR_r+P_AGE_r,AFdrv.boxplot.data,FUN=function(x){length(unique(x))})

# CREDIT: https://stackoverflow.com/questions/26794236/ggplot2-3d-bar-plot

library(latticeExtra)

x11()
cloud(occurID~C_HOUR_r+P_AGE_r, NAF_agg, panel.3d.cloud=panel.3dbars, col.facet='grey', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")),
      zlab='NAF Drivers')

x11()
cloud(occurID~C_HOUR_r+P_AGE_r, AF_agg, panel.3d.cloud=panel.3dbars, col.facet='grey', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")),
      zlab='AF Drivers')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@ SAND BOX @@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 
accid.sev.tbl <- data.frame('year' = 1999:2014,
                            'fatal'= length(unique(accid.cln[accid.cln$C_YEAR=1999:2014&accid.cln$C_SEV='1'])$occurID),
                            'non-fatal'= length(unique(accid.cln[accid.cln$C_YEAR=1999:2014&accid.cln$C_SEV='2'])$occurID)
                            )
                           
                             
x11()
ggplot(accid.cln, aes(x=C_YEAR, y=unique(occurID), fill=C_SEV)) +
  geom_histogram()

# Check how many records with P_ISEV=1, have same occurID as P_ISEV=2 or 3?
(sum(subset(accid.cln,accid.cln$P_ISEV=='1')$occurID %in%
     unique(subset(accid.cln,accid.cln$P_ISEV=='2'|accid.cln$P_ISEV=='3')$occurID)))/
nrow(subset(accid.cln,accid.cln$P_ISEV=='1'))
# 100%
# In other words, there are no collisions in the cleaned dataset, where nobody was reported 
# injured. Thus, we can stick with using C_SEV as is. No need to seperate Non-Fatal (2) into
# Injured or Not-Injured.



#Check if User consistent with Position. ex, All User=Driver, Position=Driver (as oppose to 
#front or rear Passenger)
#
sort(unique(subset(accid.cln,accid.cln$P_USER=='1')$P_PSN)) #Good
sort(unique(subset(accid.cln,accid.cln$P_USER=='2')$P_PSN)) #Good
sort(unique(subset(accid.cln,accid.cln$P_USER=='3')$P_PSN)) #Suspect
sort(unique(subset(accid.cln,accid.cln$P_USER=='4')$P_PSN)) #Suspect
sort(unique(subset(accid.cln,accid.cln$P_USER=='5')$P_PSN)) #Suspect
sort(unique(subset(accid.cln,accid.cln$P_USER=='U')$P_PSN)) #Suspect




###########################################################################
#####################  Section 4: DATA ANALYSIS  ##########################
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
