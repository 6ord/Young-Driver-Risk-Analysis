

# sc <- spark_connect(master = "local")
# 
# sp.accid <- spark_read_csv(sc,name = "accid_tbl"
#                            ,path = "F:/Marsh/canadian-car-accidents-19942014/NCDB_1999_to_2014.csv"
#                            ,header = TRUE
#                           #,stringsAsFactors = FALSE
#                             )
# select(accid_tbl,C_YEAR)
# 
accid <- #as.data.table(
  read.csv(".\\canadian-car-accidents-19942014\\NCDB_1999_to_2014.csv"
           , header = TRUE
           , stringsAsFactors = FALSE
  )
#               )

###########################################################################
#####################  Section 2: DATA CLEANING  ##########################
###########################################################################


accid$occurID <- paste(accid$C_YEAR,accid$C_MNTH,accid$C_WDAY,accid$C_HOUR,'_',
                       accid$C_RCFG,accid$C_WTHR,accid$C_RSUR,accid$C_RALN,
                       accid$C_TRAF,'_',
                       accid$C_CONF,accid$C_VEHS,accid$C_SEV,
                       sep='')

accid$vehicID <- paste(accid$occurID,'_',accid$V_ID,
                       accid$V_TYPE,accid$V_YEAR,
                       sep='')

# Save Count of vehIDs within occID to numVehs
accid$numVehs <- as.character(ave(accid$vehicID,accid$occurID,FUN=function(x)length(unique(x))))
accid$numVehs <- ifelse(nchar(accid$numVehs)<2,paste('0',accid$numVehs,sep=''),accid$numVehs)

accid$persnID <- paste(accid$vehicID,'_',accid$P_ID,
                       accid$P_PSN,accid$P_ISEV,
                       accid$P_SAFE,accid$P_USER,
                       sep='')

# MISSING if any of above ID contains U|X, --OR-- Age of driver unknown
# Applicable to Occurences - will remove Occurence if any are TRUE
missingVal <- c('UU','XX','U','X')
vehType <- c('01','05','06')    #Private passenger, vans, light trucks

accid$exclude <- (regexpr('U|X',accid$persnID)>0)|          #Missing time, road condition, injury info, etc
                         (accid$P_ISEV=='N')|
                         (accid$P_USER=='1' & accid$P_AGE %in% missingVal)|  #Missing Age of Driver
                        #|(accid$numVehs!=accid$C_VEHS)                      #Original Veh COunt doesn't match unique num of IDs  
                         !(accid$V_TYPE %in% vehType)                        #Vehicle is not priv passenger, van or light truck

# To Remove entire Occurences if it contains at least 1 record with
# missing key info ($exclude=TRUE):
# First generate list of occurIDs to remove
occurToRemove <- unique(accid$occurID[which(accid$exclude==TRUE)])
# REMOVE if occurID is on the above list
accid.cln <- accid[which(!(accid$occurID %in% occurToRemove)),][,1:(ncol(accid)-1)]

source('accid_range.r') #build bands/ranges for age, time of day, time of week, rd/crash config, etc

expl.fields.r <- colnames(accid.cln[27:32])
asso.fields.r <- c(expl.fields.r,colnames(accid.cln[20]))

# Passenger / Driver Linkage *********************************************
# Building DRV_AGE:
# - Build temporary dataframe of unique vehicle IDs and its driver's age ('accid.drvs'),
#   remove vehicle IDs where there were multiple drivers (remove data anomalies), then
#   lastly, merge accid.drvs with original cleaned dataset.
#   
driver.fields <- c('C_YEAR','P_USER','vehicID',
                   'P_AGE',asso.fields.r)
accid.drvs <- accid.cln[accid.cln$P_USER=='1'& accid.cln$P_AGE!='NN',]

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
before <- nrow(accid.drvs)
accid.drvs <- accid.drvs[which(!accid.drvs$vehicID %in% data.frame(tempTab[tempTab>1])[,1]),]
#number of vehicles removed
before-nrow(accid.drvs)
(before-nrow(accid.drvs))/before
rm(tempTab,before)

#Check vehicle ID's unique in accid.drvs set
nrow(accid.drvs)==length(unique(accid.drvs$vehicID))
#Check vehicle ID's all have only 1 driver
nrow(accid.drvs)==sum(as.numeric(accid.drvs$P_USER))
# MERGE
accid.cln.wDrvAge <- merge(accid.drvs[c('vehicID','P_AGE','P_AGE_r')],
                           accid.cln,
                           by='vehicID',
                           all.y=TRUE)
 
# Tidy column names
newFields <- colnames(accid.cln.wDrvAge)
newFields[2:3] <- c('DRV_AGE','DRV_AGE_r')
newFields[21] <- c('P_AGE')
newFields[29] <- c('P_AGE_r')
colnames(accid.cln.wDrvAge) <- newFields

# Need this for later
asso.fields.r <- c(asso.fields.r,colnames(accid.cln.wDrvAge[3]))

# Left Join from MERGE yielded 202,111 of 2.52MM records with NULL
# DRV_AGE. (There was no driver age found)
# 
sum(is.na(accid.cln.wDrvAge$DRV_AGE))
sum(!is.na(accid.cln.wDrvAge$DRV_AGE))
# Remove them:
accid.cln.wDrvAge <- accid.cln.wDrvAge[which(!is.na(accid.cln.wDrvAge$DRV_AGE)),]

rm(driver.fields,accid.drvs
   ,accid.cln
)

#Reducing the dataset of individual vehicle occupants into a
#dataset of accidnet occurrences. 
source('accid_reduceToOccur.r')

#Randomize order of records of both datasets for modelling.
#I take the first XXXXX records of datasets as a method
#of random sampling.
set.seed(9850)
randomOccSeq <- runif(nrow(accid.cln.occur))
randomIndSeq <- runif(nrow(accid.cln.wDrvAge))

accid.cln.occur <- accid.cln.occur[order(randomOccSeq),]
accid.cln.wDrvAge <- accid.cln.wDrvAge[order(randomIndSeq),]

# BUILDING FUNCTIONS
# myBarplot Function
# Credit with plotting the Legend here:
# https://coders-corner.net/2016/03/06/data-visualization-in-r-show-legend-outside-of-the-plotting-area/
#
myBarplot <- function(main='no title',bar,grp,numBars=NA){
  if(is.na(numBars)){numBars=length(unique(bar))}
  # barplot(table(bar,grp),
  #         border=NA,
  #         beside=TRUE
  #         )
  # grid(col='light grey')
  barplot(table(bar,grp),
          main=main,
          #xlab='P_ISEV',
          ylab='Frequency',
          col=rainbow(numBars),
          border=NA,
          beside=TRUE,
          legend.text=sort(unique(bar)),
          args.legend = list(x ='right',inset=-0.1,xpd=TRUE)
  )
}
# myLineplot Function
# 
myLineplot <- function(main='no title',xLab='xlabel',yLab='Frequency',indep,grp){
  ggplot(data=data.frame(table(xaxis=indep,lines=grp)),
         aes(x=xaxis
            ,y=Freq
            ,group=lines
            ,colour=lines
            #,shape=sex
            )) +
    geom_line() +
    #geom_bar()+
    #geom_point() +
    xlab(xLab) +
    ylab(yLab) +
    ggtitle(main)
}

# x11()
#test <- data.frame(table(yr=accid.cln.occur$C_YEAR,age=accid.cln.occur$mnDRV_AGE_r))
#ggplot(data=test,aes(x=yr,y=Freq,group=age,colour=age))+geom_line()+geom_point()
#
# myLineplot(main='no title',xLab='xlabel',indep=accid.cln.occur$C_YEAR,grp=accid.cln.occur$mnDRV_AGE_r)