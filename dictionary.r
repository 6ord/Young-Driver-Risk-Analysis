
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
