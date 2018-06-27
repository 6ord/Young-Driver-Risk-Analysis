rm(list=ls())

# install.packages("sparklyr")
# library(sparklyr)

# install.packages("data.table")
# library(data.table)
# Tried data.table, no noticable difference in run times. Code is simpler though.

setwd('F:\\Ryerson\\CKME136_Capstone\\repository')

source('accid_base.r')
source('accid_range.r')

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

###########################################################################
###################  Section 3: PRELIM DATA TRENDS  #######################
###########################################################################


#Weather and Rd Surface redundent? (C_WTHR vs C_RSUR)
#
rdVsWthr <- as.data.frame(cbind(road=accid.cln$C_RSUR,
                                wthr=accid.cln$C_WTHR))
#group some roads so have same length as wther
rdVsWthr[which(rdVsWthr$road %in% c('7','8')),]$road <- '6'

rdVsWthr$road = factor(rdVsWthr$road,
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


## Another Data Summary Table for cleaned data
##
rm(defitn.tbl,defitn.tbl.cln,dataDictionary,i)


##### Explore Association Rules
#####
 
expl.fields.r <- colnames(accid.cln[28:33])
asso.fields.r <- c(expl.fields.r,colnames(accid.cln[20]))

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


# MAKE SET OF DATA ONLY WITH FATALITIES AND REBUILD RULES
# CLEAN UP WITH SOME SOURCING


#Check 100 random records
View(accid.cln[sample(1:nrow(accid.cln),100),])

#Count stuff - By P_ISEV Every Year
#
#(old)aggregate(persnID~P_ISEV+C_YEAR, data=accid.cln, length)
#(old)aggregate(persnID~C_YEAR+C_WDAY_r, data=subset(accid.cln,accid.cln$P_ISEV=='2'), length)

#P_ISEV Comparison
x11()
barplot(table(accid.cln$P_ISEV,accid.cln$C_YEAR),
        main='Counts of Not Injured (1), Injured (2) and Fatal (3)',
        xlab='P_ISEV',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln$P_ISEV,accid.cln$C_YEAR)),
        beside=TRUE)

# Deep Dive in Fatal only
x11()
barplot(table(x=factor(accid.cln$P_ISEV,exclude=c('1','2')),accid.cln$C_YEAR),
        main='Counts of Fatal (3)',
        xlab='P_ISEV=3',
        ylab='Frequency'
        #col=rainbow,
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
barplot(table(accid.cln.fatal$C_HOUR_r,accid.cln.fatal$C_WDAY_r),
        main='Fatal Injuries by Time',
        xlab='Fatal',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.fatal$C_HOUR_r,accid.cln.fatal$C_WDAY_r)),
        beside=TRUE)

# Injuries by Time
x11()
barplot(table(accid.cln.injrd$C_HOUR_r,accid.cln.injrd$C_WDAY_r),
        main='Injuries by Time',
        xlab='Injured',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.injrd$C_HOUR_r,accid.cln.injrd$C_WDAY_r)),
        beside=TRUE)

# Fatal Injuries by Rd Config
x11()
barplot(table(accid.cln.fatal$C_RCFG_r,accid.cln.fatal$C_YEAR),
        main='Fatal Injuries by Rd Config',
        xlab='Fatal',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.fatal$C_RCFG_r,accid.cln.fatal$C_YEAR)),
        beside=TRUE)

# Injuries by Rd Config
x11()
barplot(table(accid.cln.injrd$C_RCFG_r,accid.cln.injrd$C_YEAR),
        main='Injuries by Rd Config',
        xlab='Injured',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.injrd$C_RCFG_r,accid.cln.injrd$C_YEAR)),
        beside=TRUE)


# Fatal Injuries by Traffic Control
x11()
barplot(table(accid.cln.fatal$C_TRAF_r,accid.cln.fatal$C_YEAR),
        main='Fatal Injuries by Traffic Control',
        xlab='Fatal',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.fatal$C_TRAF_r,accid.cln.fatal$C_YEAR)),
        beside=TRUE)

# Injuries by Traffic Control
x11()
barplot(table(accid.cln.injrd$C_TRAF_r,accid.cln.injrd$C_YEAR),
        main='Injuries by Traffic Control',
        xlab='Injured',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.injrd$C_TRAF_r,accid.cln.injrd$C_YEAR)),
        beside=TRUE)

# Fatal Injuries by Collision Config
x11()
barplot(table(accid.cln.fatal$C_CONF_r,accid.cln.fatal$C_YEAR),
        main='Fatal Injuries by Collision Config',
        xlab='Fatal',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.fatal$C_CONF_r,accid.cln.fatal$C_YEAR)),
        beside=TRUE)

# Injuries by Collision Config
x11()
barplot(table(accid.cln.injrd$C_CONF_r,accid.cln.injrd$C_YEAR),
        main='Injuries by Collision Config',
        xlab='Injured',
        ylab='Frequency',
        #col=rainbow,
        legend=rownames(table(accid.cln.injrd$C_CONF_r,accid.cln.injrd$C_YEAR)),
        beside=TRUE)



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@ COLORS, FRIDAY, COUNT ACCID(not ppl) @@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

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
