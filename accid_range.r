
###########################################################################
###############  Section 3: PRELIM DATA TRENDS  (setup)  ##################
###########################################################################
# - C_VEHS & C_CONF NOT CONSISTENT!!!

# Setting P_ISEV as dependent variable, needs to be factor.
accid.cln$P_ISEV <- as.factor(accid.cln$P_ISEV)

# Build Age Groups
accid.cln$P_AGE_r <- cut(as.numeric(accid.cln$P_AGE),
                         breaks = c(0,17,26,36,50,65,99),
                         labels = c('0-16','17-25','26-35','36-49','50-64','65+'),
                         right=FALSE)
# Build Time of Day
accid.cln$C_HOUR_r <- cut(as.numeric(accid.cln$C_HOUR),
                          breaks = c(0,5,8,10,12,14,17,19,21,24),
                          labels = c('overnight','AM_early','AM_rush','AM_late','mid_day',
                                     'PM','PM_rush','PM_evening','PM_late'),
                          right=FALSE)

# Build Time of week
accid.cln$C_WDAY_r <- cut(as.numeric(accid.cln$C_WDAY),
                          breaks = c(1,6,8),
                          labels = c('wkdy','wknd'),
                          right=FALSE)

# Build Collision Config
accid.cln$C_CONF_temp <- accid.cln$C_CONF
accid.cln$C_CONF_temp <- gsub('41','00',accid.cln$C_CONF_temp)
accid.cln$C_CONF_temp <- gsub('QQ','88',accid.cln$C_CONF_temp)
accid.cln$C_CONF_r <- cut(as.numeric(accid.cln$C_CONF_temp),
                          breaks = c(0,3,6,21,31,88,99),
                          labels = c('single_colln','single_lctrl','single_othr',
                                     'two_OneDir','two_multiDir','other'),
                          right=FALSE)
accid.cln$C_CONF_temp <- NULL


# Build road Config
# CREDIT: https://stackoverflow.com/questions/19441092/how-can-i-create-an-infix-between-operator
# 
'%bw%'<-function(x,rng) x>=rng[1] & x<=rng[2]

accid.cln$C_RCFG_temp <- accid.cln$C_RCFG
accid.cln$C_RCFG_temp <- gsub('QQ','99',accid.cln$C_RCFG_temp)
accid.cln$C_RCFG_temp <- as.numeric(accid.cln$C_RCFG_temp)
accid.cln$C_RCFG_r <- ''

accid.cln[which(accid.cln$C_RCFG_temp %bw% c(2,3)),]$C_RCFG_r <- 'intersctn'
accid.cln[which(accid.cln$C_RCFG_temp %bw% c(4,6)|accid.cln$C_RCFG_temp==1),]$C_RCFG_r <- 'city'
accid.cln[which(accid.cln$C_RCFG_temp==7),]$C_RCFG_r <- 'passLane'
accid.cln[which(accid.cln$C_RCFG_temp==8),]$C_RCFG_r <- 'ramp'
accid.cln[which(accid.cln$C_RCFG_temp==9),]$C_RCFG_r <- 'traffCrcle'
accid.cln[which(accid.cln$C_RCFG_temp %bw% c(10,12)),]$C_RCFG_r <- 'hwy'
accid.cln[which(accid.cln$C_RCFG_temp==99),]$C_RCFG_r <- 'other'
accid.cln$C_RCFG_temp <- NULL
accid.cln$C_RCFG_r <- as.factor(accid.cln$C_RCFG_r)

# Build traffic Control
# 
accid.cln$C_TRAF_temp <- accid.cln$C_TRAF
accid.cln$C_TRAF_temp <- gsub('QQ','99',accid.cln$C_TRAF_temp)
accid.cln$C_TRAF_temp <- as.numeric(accid.cln$C_TRAF_temp)
accid.cln$C_TRAF_r <- ''

accid.cln[which(accid.cln$C_TRAF_temp %bw% c(3,6)|
                  accid.cln$C_TRAF_temp %bw% c(9,12)),]$C_TRAF_r <- 'signage'
accid.cln[which(accid.cln$C_TRAF_temp %bw% c(7,8)),]$C_TRAF_r <- 'person'
accid.cln[which(accid.cln$C_TRAF_temp %bw% c(13,14)),]$C_TRAF_r <- 'schBus'
accid.cln[which(accid.cln$C_TRAF_temp %bw% c(15,16)),]$C_TRAF_r <- 'rail'
accid.cln[which(accid.cln$C_TRAF_temp==18),]$C_TRAF_r <- 'none'
accid.cln[which(accid.cln$C_TRAF_temp==17|
                  accid.cln$C_TRAF_temp==99),]$C_TRAF_r <- 'other'

accid.cln[which(accid.cln$C_TRAF_temp==1),]$C_TRAF_r <- 'lights_op'
accid.cln[which(accid.cln$C_TRAF_temp==2),]$C_TRAF_r <- 'lights_np'

accid.cln$C_TRAF_temp <- NULL
accid.cln$C_TRAF_r <- as.factor(accid.cln$C_TRAF_r)



