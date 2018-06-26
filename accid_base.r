# install.packages("pdftools")
library(pdftools)
# install.packages("arules")
library(arules)
library(ggplot2)
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
                         (accid$P_USER=='1' & accid$P_AGE %in% missingVal)  #Missing Age of Driver
                        #|(accid$numVehs!=accid$C_VEHS)                      #Original Veh COunt doesn't match unique num of IDs  
                        #|!(accid$V_TYPE %in% vehType)                        #Vehicle is not priv passenger, van or light truck

# To Remove entire Occurences if it contains at least 1 record with
# missing key info ($exclude=TRUE):
# First generate list of occurIDs to remove
occurToRemove <- unique(accid$occurID[which(accid$exclude==TRUE)])
# REMOVE if occurID is on the above list
accid.cln <- accid[which(!(accid$occurID %in% occurToRemove)),]

