rm(list=ls())

# install.packages("sparklyr")
# library(sparklyr)

# install.packages("data.table")
# library(data.table)
# Tried data.table, no noticable difference in run times. Code is simpler though.

# install.packages("pdftools")
library(pdftools)
# install.packages("arules")
library(arules)
# install.packages("C50")
#library(C50)
# install.packages("rpart")
#library(rpart)
#library(ggplot2)
# spark_install(version = "2.1.0")
# devtools::install_github("rstudio/sparklyr")


#setwd('F:\\Ryerson\\CKME136_Capstone\\repository')
setwd('C:\\Users\\trunk\\Downloads\\BigData\\CKME136_Capstone\\repository')

#source('dictionary.r') #builds data dictionary from PDF
#ALSO UNCOMMENT LINES XX-XX IF BUILDING DICTIONARY **********************************************
#
#View(defitn.tbl)

startTime <- Sys.time()
source('accid_base.r') #packages, importing, build occur, veh and person IDs
data.frame(start=startTime,end=Sys.time(),runtime=(Sys.time()-startTime))
rm(startTime)

#####################################################
# SKIP TO LINE 135 to begin subsetting and plotting #
#####################################################

# There's NA's coherced from banding age. The below ensures at least all
# drivers have age and not NA.
sum(is.na(accid.cln.wDrvAge$P_AGE_r)&(accid.cln.wDrvAge$P_USER!='1'))/
  sum(is.na(accid.cln.wDrvAge$P_AGE_r))
# OR
sum(is.na(accid.cln.wDrvAge$DRV_AGE))

###########################################################################
#####################  Section 2: DATA CLEANING  ##########################
###########################################################################
# Original dataset had count of vehicles in accident. Check if above step yielded same
# vehicle counts.
nrow(subset(accid,accid$numVehs!=accid$C_VEHS))#/nrow(accid)
# 1.97M records did not match, out of 5.8MM -  34%

# Quick Metrics
# 
length(unique(accid$occurID[which(accid$exclude==TRUE)]))/length(unique(accid$occurID))
# 49.85% of occurrence IDs will be excluded
nrow(subset(accid,accid$occurID %in% occurToRemove))/nrow(accid)
# 53.62% of all records will be removed

nrow(accid.cln)
length(unique(accid.cln$occurID))
# 2.72MM records
# 928K occurences, spread reasonably even throughout years - as per below
for (i in 1999:2014){print(length(unique(accid.cln$occurID[which(accid.cln$C_YEAR==i)])))}

## Another Data Summary Table for cleaned data
##
# defitn.tbl.cln <- defitn.tbl
# for (i in 1:22){defitn.tbl.cln$values[i] <- unique(accid.cln[i])
# defitn.tbl.cln$numNA[i] <- sum(accid.cln[,i] %in% missingVal)
# }
# View(defitn.tbl.cln)
# 
# ## Another Data Summary Table for cleaned data
# ##
# rm(defitn.tbl,defitn.tbl.cln,dataDictionary,i)

###########################################################################
###################  Section 3: PRELIM DATA TRENDS  #######################
###########################################################################

# Test Review some records. Ensure passengers' driver age is correct. (DRV_AGE
# should equal the P_AGE of occupant with P_USER=1 and same vehicID.)

# Randomly take 10 sequential Vehicle ID records to examine. Ensure driver age
# was captured on passenger records.
uniqueVehIDs <- unique(accid.cln.wDrvAge$vehicID)
raNum <- sample(11:length(uniqueVehIDs)-11,1)
sampleVehID <- uniqueVehIDs[(raNum-10):raNum]
rm(uniqueVehIDs,raNum)
#View(sampleVehID) #Random 10 consequtive Vehicle IDs
View(subset(accid.cln.wDrvAge,accid.cln.wDrvAge$vehicID==sampleVehID[9])[c('vehicID','P_USER',
                                                                           'P_AGE','DRV_AGE',
                                                                           'DRV_AGE_r')])
rm(sampleVehID)

###################################
####### Frequency Plots ###########
####### -By Injury Line XXX #######
####### -By driverAge Line XXX ####
###################################

# Side Study: Weather and Rd Surface redundent? (C_WTHR vs C_RSUR)
#
rdVsWthr <- as.data.frame(cbind(road=accid.cln.wDrvAge$C_RSUR,
                                wthr=accid.cln.wDrvAge$C_WTHR))
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

####################################################
####### Frequency Plots: All by Injury Level #######
####################################################
# SKIP TO LINE 244 for Frequency Plots: All by DriverAge

#Create Subsets by Injury Level
#
accid.cln.fatal <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$P_ISEV=='3')
accid.cln.injrd <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$P_ISEV=='2')
accid.cln.noinj <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$P_ISEV=='1')
# Check
nrow(accid.cln.wDrvAge)==sum(nrow(accid.cln.fatal),
                             nrow(accid.cln.injrd),
                             nrow(accid.cln.noinj)
                             )

#P_ISEV Comparison
x11()
par(mfrow=c(1,2))
myBarplot('Counts of Not Injured (1), Injured (2) and Fatal (3)',
          bar=accid.cln.wDrvAge$P_ISEV,grp=accid.cln.wDrvAge$C_YEAR)
# Deep Dive in Fatal only
myBarplot('Counts of Fatal (3)',
          bar=factor(accid.cln.wDrvAge$P_ISEV,exclude=c('1','2')),
          grp=accid.cln.wDrvAge$C_YEAR,1)

# Fatal, Non-Fatal, and No Injuries by Time
# 
x11()
par(mfrow=c(1,3))
myBarplot('Fatal Injuries by Time',accid.cln.fatal$C_HOUR_r,accid.cln.fatal$C_WDAY_r)
myBarplot('Injuries by Time',accid.cln.injrd$C_HOUR_r,accid.cln.injrd$C_WDAY_r)
myBarplot('No Injuries by Time',accid.cln.noinj$C_HOUR_r,accid.cln.noinj$C_WDAY_r)

#What if we count Friday night as weekend, and Sunday night as weekday??
#
fields.friday.r <- c(colnames(accid.cln[3]),expl.fields.r,colnames(accid.cln[20]))
temp.accid <- accid.cln[fields.friday.r]
temp.accid.fatal <- subset(accid.cln,accid.cln$P_ISEV=='3')
temp.accid.injrd <- subset(accid.cln,accid.cln$P_ISEV=='2')
temp.accid.noinj <- subset(accid.cln,accid.cln$P_ISEV=='1')
# Fatal Injuries
x11()
par(mfrow=c(1,2))
myBarplot('Fatal Injuries by Day (Mon-Sun)',temp.accid.fatal$C_HOUR_r,temp.accid.fatal$C_WDAY)
myBarplot('Injuries by Day (Mon-Sun)',temp.accid.injrd$C_HOUR_r,temp.accid.injrd$C_WDAY)

######################################################################
# CONCLUSION: Friday night/overnight indeed has a lot. Surprisingly, #
# Sunday overnight was high too.                                     #
######################################################################
rm(temp.accid,temp.accid.fatal,temp.accid.injrd,temp.accid.noinj, fields.friday.r)

# Fatalities, Injuries and non-injuries by Driver Age
# 
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot("Fatal Injuries by their Drivers' Age",accid.cln.fatal$DRV_AGE_r,accid.cln.fatal$C_YEAR)
myBarplot("Injuries by their Drivers' Age",accid.cln.injrd$DRV_AGE_r,accid.cln.injrd$C_YEAR)
myBarplot("Non-Injuries by their Drivers' Age",accid.cln.noinj$DRV_AGE_r,accid.cln.noinj$C_YEAR)

# Fatalities, Injuries and non-injuries by Rd Config
# 
x11()
#par(mfrow=c(1,3))
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Injuries by Rd Config',accid.cln.fatal$C_RCFG_r,accid.cln.fatal$C_YEAR)
myBarplot('Injuries by Rd Config',accid.cln.injrd$C_RCFG_r,accid.cln.injrd$C_YEAR)
myBarplot('No Injuries by Rd Config',accid.cln.noinj$C_RCFG_r,accid.cln.noinj$C_YEAR)

# Fatalities, Injuries and non-injuries by Traffic Control
# 
x11()
#par(mfrow=c(1,3))
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Injuries by Traffic Control',accid.cln.fatal$C_TRAF_r,accid.cln.fatal$C_YEAR)
myBarplot('Injuries by Traffic Control',accid.cln.injrd$C_TRAF_r,accid.cln.injrd$C_YEAR)
myBarplot('No Injuries by Traffic Control',accid.cln.noinj$C_TRAF_r,accid.cln.noinj$C_YEAR)

# Fatalities, Injuries and non-injuries by Collision Config
# 
x11()
#par(mfrow=c(1,3))
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Injuries by Collision Config',accid.cln.fatal$C_CONF_r,accid.cln.fatal$C_YEAR)
myBarplot('Injuries by Collision Config',accid.cln.injrd$C_CONF_r,accid.cln.injrd$C_YEAR)
myBarplot('No Injuries by Collision Config',accid.cln.noinj$C_CONF_r,accid.cln.noinj$C_YEAR)

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
# CONCLUSION: Still high in 1999. But noticed Single Other was very low - suspect
# recoding from 'Single Other' to 'Single Collision' after 1999
rm(temp.accid.uniq)

rm(accid.cln.fatal,
   accid.cln.injrd,
   accid.cln.noinj
   )

#################################################
####### Frequency Plots: All by DriverAge #######
#################################################
# SKIP TO LINE 302 FOR PER OCCURRENCE DISTRIBUTIONS

#High Level DrvAge Comparison
zoom17To49 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE %bw% c(17,49))
x11()
par(mfrow=c(2,1))
myBarplot('Counts of Colliding Vehicle Occupants by Driver Age',accid.cln.wDrvAge$DRV_AGE_r,accid.cln.wDrvAge$C_YEAR,6)
# Zoom in between 17-49
myBarplot('Counts of Colliding Vehicle Occupants by Driver Age',zoom17To49$DRV_AGE,zoom17To49$C_YEAR)
rm(zoom17To49)

#Create Subsets by Age Group
#
accid.cln.17 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE_r=='17-25')
accid.cln.26 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE_r=='26-35')
accid.cln.36 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE_r=='36-49')

# Occupants by 3 top age groups by Time
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Occupants of Drv Age 17-25 by Time',accid.cln.17$C_HOUR_r,accid.cln.17$C_WDAY_r)
myBarplot('Occupants of Drv Age 26-35 by Time',accid.cln.26$C_HOUR_r,accid.cln.26$C_WDAY_r)
myBarplot('Occupants of Drv Age 36-49 by Time',accid.cln.36$C_HOUR_r,accid.cln.36$C_WDAY_r)

# Top 3 Age Groups by Rd Config
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Occupants of Drv Age 17-25 by Rd Config',accid.cln.17$C_RCFG_r,accid.cln.17$C_YEAR)
myBarplot('Occupants of Drv Age 26-35 by Rd Config',accid.cln.26$C_RCFG_r,accid.cln.26$C_YEAR)
myBarplot('Occupants of Drv Age 36-49 by Rd Config',accid.cln.36$C_RCFG_r,accid.cln.36$C_YEAR)

# Top 3 Age Groups by Traffic Control
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Occupants of Drv Age 17-25 by Traffic Control',accid.cln.17$C_TRAF_r,accid.cln.17$C_YEAR)
myBarplot('Occupants of Drv Age 26-35 by Traffic Control',accid.cln.26$C_TRAF_r,accid.cln.26$C_YEAR)
myBarplot('Occupants of Drv Age 36-49 by Traffic Control',accid.cln.36$C_TRAF_r,accid.cln.36$C_YEAR)

# Top 3 Age Groups by Collision Configuration
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Occupants of Drv Age 17-25 by Collision Config',accid.cln.17$C_CONF_r,accid.cln.17$C_YEAR)
myBarplot('Occupants of Drv Age 26-35 by Collision Config',accid.cln.26$C_CONF_r,accid.cln.26$C_YEAR)
myBarplot('Occupants of Drv Age 36-49 by Collision Config',accid.cln.36$C_CONF_r,accid.cln.36$C_YEAR)

rm(accid.cln.17,accid.cln.26,accid.cln.36)

###########################################################################
###########################################################################
################################   REPEAT  ################################                     
################################   ABOVE   ################################
################################   w.      ################################
################################   REDUCED ################################
###########################################################################
###########################################################################

#Plotting Subsets by Injury Level
#SKIP TO LINE 377 FOR SUBSETS BY DRV AGE GROUP
#
occur.fatal <- subset(accid.cln.occur,accid.cln.occur$collSev=='fatal')
occur.injry <- subset(accid.cln.occur,accid.cln.occur$collSev=='injry')
occur.noInj <- subset(accid.cln.occur,accid.cln.occur$collSev=='noInj')
# Check
nrow(accid.cln.occur)==sum(nrow(occur.fatal),
                           nrow(occur.injry),
                           nrow(occur.noInj)
                          )

x11()
#par(mfrow=c(1,2))
par(mfrow=c(2,1),mar = c(5,4,4,8))
myBarplot('Collision Count by Severity',
          bar=accid.cln.occur$collSev,grp=accid.cln.occur$C_YEAR)
# Deep Dive in Fatal only
# myBarplot('Fatal Collision Count',
#           bar=factor(accid.cln.occur$collSev,exclude=c('noInj','injry')),
#           grp=accid.cln.occur$C_YEAR,1)
myBarplot('Collision Count by Youngest Driver Age',
          bar=accid.cln.occur$mnDRV_AGE_r,grp=accid.cln.occur$C_YEAR)

# Fatal, Non-Fatal, and No Injuries by Min Drv Age
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Collisions by Min Drv Age',occur.fatal$mnDRV_AGE_r,occur.fatal$C_YEAR)
myBarplot('Injury Collisions by Min Drv Age',occur.injry$mnDRV_AGE_r,occur.injry$C_YEAR)
myBarplot('Non-Injury Collisions by Min Drv Age',occur.noInj$mnDRV_AGE_r,occur.noInj$C_YEAR)

# Fatal, Non-Fatal, and No Injuries by Time
x11()
#par(mfrow=c(1,3))
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Collisions by Time',occur.fatal$C_HOUR_r,occur.fatal$C_WDAY_r)
myBarplot('Injury Collisions by Time',occur.injry$C_HOUR_r,occur.injry$C_WDAY_r)
myBarplot('Non-Injury Collisions by Time',occur.noInj$C_HOUR_r,occur.noInj$C_WDAY_r)

x11()
#par(mfrow=c(1,3))
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Collisions by Rd Config',occur.fatal$C_RCFG_r,occur.fatal$C_YEAR)
myBarplot('Injury Collisions by Rd Config',occur.injry$C_RCFG_r,occur.injry$C_YEAR)
myBarplot('Non-Injury Collisions by Rd Config',occur.noInj$C_RCFG_r,occur.noInj$C_YEAR)

# Fatalities, Injuries and non-injuries by Traffic Control
x11()
#par(mfrow=c(1,3))
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Collisions by Traffic Control',occur.fatal$C_TRAF_r,occur.fatal$C_YEAR)
myBarplot('Injury Collisions by Traffic Control',occur.injry$C_TRAF_r,occur.injry$C_YEAR)
myBarplot('Non-Injury Collisions by Traffic Control',occur.noInj$C_TRAF_r,occur.noInj$C_YEAR)

# Fatalities, Injuries and non-injuries by Collision Config
x11()
#par(mfrow=c(1,3))
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Fatal Collisions by Config',occur.fatal$C_CONF_r,occur.fatal$C_YEAR)
myBarplot('Injury Collisions by Config',occur.injry$C_CONF_r,occur.injry$C_YEAR)
myBarplot('Non-Injury Collisions by Config',occur.noInj$C_CONF_r,occur.noInj$C_YEAR)

rm(occur.fatal,
   occur.injry,
   occur.noInj
  )

# zoom17To49 <- subset(accid.cln.wDrvAge,accid.cln.wDrvAge$DRV_AGE %bw% c(17,49))
# x11()
# par(mfrow=c(2,1))
# myBarplot('Counts of Colliding Vehicle Occupants',accid.cln.wDrvAge$DRV_AGE_r,accid.cln.wDrvAge$C_YEAR,6)
# # Zoom in between 17-49
# myBarplot('Counts of Colliding Vehicle Occupants',zoom17To49$DRV_AGE,zoom17To49$C_YEAR)
# rm(zoom17To49)

#Plotting Subsets by Min Drv Age Group
#SKIP TO LINE 422 for Modelling (chisq test, assoc rules)
#
occur.17 <- subset(accid.cln.occur,accid.cln.occur$mnDRV_AGE_r=='17-25')
occur.26 <- subset(accid.cln.occur,accid.cln.occur$mnDRV_AGE_r=='26-35')
occur.36 <- subset(accid.cln.occur,accid.cln.occur$mnDRV_AGE_r=='36-49')

x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Collisions w/ Youngest Drv Age 17-25 by Time',occur.17$C_HOUR_r,occur.17$C_WDAY_r)
myBarplot('Collisions w/ Youngest Drv Age 26-35 by Time',occur.26$C_HOUR_r,occur.26$C_WDAY_r)
myBarplot('Collisions w/ Youngest Drv Age 36-49 by Time',occur.36$C_HOUR_r,occur.36$C_WDAY_r)

# Top 3 Age Groups by Rd Config
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Collisions w/ Youngest Drv Age 17-25 by Rd Config',occur.17$C_RCFG_r,occur.17$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 26-35 by Rd Config',occur.26$C_RCFG_r,occur.26$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 36-49 by Rd Config',occur.36$C_RCFG_r,occur.36$C_YEAR)

# Top 3 Age Groups by Collision Severity
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Collisions w/ Youngest Drv Age 17-25 by Collision Severity',occur.17$collSev,occur.17$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 26-35 by Collision Severity',occur.26$collSev,occur.26$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 36-49 by Collision Severity',occur.36$collSev,occur.36$C_YEAR)

# Top 3 Age Groups by Traffic Control
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Collisions w/ Youngest Drv Age 17-25 by Traffic Control',occur.17$C_TRAF_r,occur.17$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 26-35 by Traffic Control',occur.26$C_TRAF_r,occur.26$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 36-49 by Traffic Control',occur.36$C_TRAF_r,occur.36$C_YEAR)

# Top 3 Age Groups by Collision Configuration
x11()
par(mfrow=c(3,1),mar = c(5,4,4,8))
myBarplot('Collisions w/ Youngest Drv Age 17-25 by Collision Config',occur.17$C_CONF_r,occur.17$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 26-35 by Collision Config',occur.26$C_CONF_r,occur.26$C_YEAR)
myBarplot('Collisions w/ Youngest Drv Age 36-49 by Collision Config',occur.36$C_CONF_r,occur.36$C_YEAR)

rm(occur.17,occur.26,occur.36)


#Chi Square Test of Independence on the relationship between youngest
#driver of accident and collision severity. Is it decreasing?
x11()
barplot(table(accid.cln.occur$C_YEAR))
#Every year, there's at least around 40,000 accidents. I will take random
#30,000 per year. See line 131 of accid_base.r on randomizing.
for(i in 1999:2014){
  assign(paste('accid.',i,sep=''),subset(accid.cln.occur,accid.cln.occur$C_YEAR==i)[1:30000,])
}

accidYrList <- list(accid.1999,accid.2000,accid.2001,accid.2002,accid.2003,
                    accid.2004,accid.2005,accid.2006,accid.2007,accid.2008,
                    accid.2009,accid.2010,accid.2011,accid.2012,accid.2013,
                    accid.2014)

rm(accid.1999,accid.2000,accid.2001,accid.2002,accid.2003,
                      accid.2004,accid.2005,accid.2006,accid.2007,accid.2008,
                      accid.2009,accid.2010,accid.2011,accid.2012,accid.2013,
                      accid.2014)

#conduct chisq test of indep for each year, see if p-value increases, indicating less/more weight
#of age on collision severity.

AgeCollSev.chisqStats <- sapply(c(1:16),
                                function(x){
                                  as.numeric(chisq.test(table(accidYrList[[x]]$mnDRV_AGE_r,accidYrList[[x]]$collSev))$statistic)
                                  }
                                )
x11()
plot(AgeCollSev.chisqStats,
     main="Chi Sq Test between Youngest Driver of Collision and Collision Severity",
     ylab="Test Statistic",
     xlab="Year: 1999-2014")
lines(AgeCollSev.chisqStats)
#Test statistic does appear to increase over the 16 years, indicating more weight of age on collision severity.

#Association Rules

# Creates a dataset of injured or not, and fatal or not.
accid.cln.injOrNot <- accid.cln.occur
accid.cln.injOrNot$collSev <- factor(gsub('fatal','injry',accid.cln.occur$collSev),
                                     levels=c('noInj','injry'))
accid.cln.fatOrNot <- subset(accid.cln.occur,accid.cln.occur$collSev!='noInj')
accid.cln.fatOrNot$collSev <- factor(accid.cln.fatOrNot$collSev,levels=c('injry','fatal'))

#High level preview tabulation
table(accid.cln.injOrNot$collSev)
table(accid.cln.fatOrNot$collSev)

#Association for injury accidents and fatal accidents
#
inj_rules <- sort(apriori(accid.cln.injOrNot[asso.fields.r.occur]
                          ,parameter=list(supp=0.10,conf=0.5,minlen=3)
                          ,appearance=list(rhs='collSev=injry'))
                  ,by='support')
inspect(inj_rules)

fat_rules <- sort(apriori(accid.cln.fatOrNot[asso.fields.r.occur]
                          ,parameter=list(supp=0.00001,conf=0.2,minlen=3)
                          ,appearance=list(rhs='collSev=fatal'))
                  ,by='lift')
inspect(fat_rules)

# Decision Tree to Weka
occTreeInput1 <- subset(accid.cln.injOrNot,
                        accid.cln.injOrNot$mnDRV_AGE_r=='17-25')[1:10000,][c('C_WDAY_r','C_RCFG_r',
                                                                             'C_TRAF_r','collSev')]

occTreeInput2 <- accid.cln.occur[c('mnDRV_AGE_r','C_WDAY_r','C_RCFG_r','C_TRAF_r','collSev')]

write.csv(occTreeInput1, file = "accidWithYoungest17to25_10KRandomRecords.csv", row.names = FALSE)
write.csv(occTreeInput2, file = "accidClnOccur_ALL.csv", row.names = FALSE)

# tm1 <- rpart(collSev ~ .,data=occTreeInput[,1:4]
#              ,method='class'
#              #,control = rpart.control(minsplit = 60, minbucket = round(minsplit/3))
#              )
# 
# x11()
# plot(tm1)
# # rpart.plot(tm1)
# # rpart.plot(tm1,type=3,extra=101)
# summary(tm1)
# 
# p1 <- predict(tm1, treeInput[200:250,],type=class) #SOMETHING WRONG HERE
# 
# table(data[200:250,5], predicted=predictModel)
# 
# # method two
# tm2 <- C5.0(occTreeInput[,-4],occTreeInput[,4])
# 
# summary(tm2)
# x11()
# plot(tm2)
# 
# pred <- predict(treeModel, accid.cln.wDrvAge[200000:300000,])
# pred
# table(accid.cln.wDrvAge[200000:300000,7], Predicted = pred)
# 
# rm(tm1,tm2,treeInput)