#######################################################
#######   Building Per Occurence data frame   #########
#######################################################

accid.cln.wDrvAge$P_ISEV <- as.numeric(accid.cln.wDrvAge$P_ISEV)

# For each occurID, get min DRV_AGE
accid.cln.occur <- aggregate.data.frame(x = accid.cln.wDrvAge[c('occurID','DRV_AGE')],
                                        by = list(accid.cln.wDrvAge$occurID),
                                        FUN = min
                                        )[c('occurID','DRV_AGE')]
# Merge above with another data frame of occurID and its
# maximum injury level
accid.cln.occur <- merge(accid.cln.occur,
                         aggregate.data.frame(x = accid.cln.wDrvAge[c('occurID','P_ISEV')],
                                              by = list(accid.cln.wDrvAge$occurID),
                                              FUN = max)[c('occurID','P_ISEV')],
                         by='occurID')

# Tidy what was built above
accid.cln.wDrvAge$P_ISEV <- as.factor(accid.cln.wDrvAge$P_ISEV)
colnames(accid.cln.occur) <- c('occurID','mnDRV_AGE','mxP_ISEV')

# Band Age Groups for Driver Age
accid.cln.occur$mnDRV_AGE_r <- cut(accid.cln.occur$mnDRV_AGE,
                                   breaks = c(0,17,26,36,50,65,100),
                                   labels = c('0-16','17-25','26-35','36-49','50-64','65+'),
                                   right=FALSE)

# Collision Severity & Order
accid.cln.occur$collSev <- cut(accid.cln.occur$mxP_ISEV,
                               breaks = c(1,2,3,99),
                               labels = c('noInj','injry','fatal'),
                               right=FALSE)
accid.cln.occur$collSev <- factor(accid.cln.occur$collSev,
                                  levels = c('noInj','injry','fatal')
                                  )
# Remove P_ISEV
accid.cln.occur$mxP_ISEV <- NULL

# Add other attributes back in
accid.cln.occur <- unique(merge(accid.cln.occur,
                                accid.cln.wDrvAge[c("C_YEAR","occurID","numVehs",
                                                    "C_HOUR_r","C_WDAY_r",
                                                    "C_CONF_r","C_RCFG_r","C_TRAF_r")],
                                by='occurID'))

accid.cln.occur$numVehs <- as.numeric(accid.cln.occur$numVehs)

# Saving Indep & Dep attributes for modelling phase
asso.fields.r.occur <- c("mnDRV_AGE_r","C_HOUR_r","C_WDAY_r","C_CONF_r",
                         "C_RCFG_r","C_TRAF_r","collSev")
