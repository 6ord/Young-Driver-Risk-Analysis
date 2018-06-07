# install.packages("sparklyr")
library(sparklyr)

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

# IMPORTING DATA

setwd('F:\\Ryerson\\CKME136_Capstone\\repository')
accid <- read.csv(".\\canadian-car-accidents-19942014\\NCDB_1999_to_2014.csv"
                  , header = TRUE
                  , stringsAsFactors = FALSE
                  )

# DATA DICTIONARY

dataDictionary <- pdf_text(".\\canadian-car-accidents-19942014\\drivingLegend.pdf")

dd.defitn <- unlist(strsplit(dataDictionary,'\n')[1])
#dd.values <- unlist(strsplit(dataDictionary,'\n')[2:11])

dd.defitn <- c(dd.defitn[3:14],
               dd.defitn[16:18],
               dd.defitn[20:26])

defitn.tbl <- as.data.frame(cbind(sapply(dd.defitn,function(x){gsub(' ','',substr(x,1,8))}),
                                  sapply(dd.defitn,function(x){substr(x,44,nchar(x)-1)})
                                  ))
rm(dd.defitn)

row.names(defitn.tbl) <- NULL
colnames(defitn.tbl) <- c('attribute','description')

defitn.tbl$values <- ''
for (i in 1:22){defitn.tbl$values[i] <- ((unique(accid[i])))}
for (i in 1:22){defitn.tbl$numNA[i] <- sum(is.na(accid[i]))}
defitn.tbl$attrType <- c('Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Qual-Ordinal','Quan-Discrete','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Ordinal','Qual-Nominal','Qual-Ordinal','Qual-Ordinal','Qual-Nominal','Qual-Ordinal','Qual-Nominal','Qual-Nominal','Qual-Nominal','Qual-Nominal')


# Exploring

# x11()
# hist(as.numeric(accid$P_AGE))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='1999')))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='2004')))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='2009')))
# hist(as.numeric(subset(accid$P_AGE,accid$C_YEAR=='2014')))

x11()
boxplot(as.numeric(accid$P_AGE)~accid$C_YEAR,accid)
# lower whisker always at 0??? remove if outlier - children vehicle occupants
summary(as.numeric(accid$P_AGE))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     1.0    22.0    34.0    36.4    49.0    99.0  395156 



rm(list=ls())
View(head(accid))
str(accid)
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