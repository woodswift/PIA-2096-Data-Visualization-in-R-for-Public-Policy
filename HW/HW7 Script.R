# load DHS data
# Easier way to import data by using package "readxl"
# File, Import Dataset, Excel (Sheet)
library(readxl)
dat <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx",
                  sheet = "DHS_Case_Clients_2016EntryCohor")

str(dat)
summary(dat)

# --- Question 1: convert date data ---

# first convert to factor type in order to check missing values
dat$CL_INLV_START <- as.factor(dat$CL_INLV_START)
dat$CL_CYF_MIN_DT <- as.factor(dat$CL_CYF_MIN_DT)
dat$SRVC_ACPT_DT_MY <- as.factor(dat$SRVC_ACPT_DT_MY)
dat$CLOSE_DT <- as.factor(dat$CLOSE_DT)

# use summary()
summary(dat$CL_INLV_START)
summary(dat$CL_CYF_MIN_DT)
summary(dat$SRVC_ACPT_DT_MY)
summary(dat$CLOSE_DT) # find missing value exists!

# convert back to string
dat$CL_INLV_START <- as.character(dat$CL_INLV_START)
dat$CL_CYF_MIN_DT <- as.character(dat$CL_CYF_MIN_DT)
dat$SRVC_ACPT_DT_MY <- as.character(dat$SRVC_ACPT_DT_MY)
dat$CLOSE_DT <- as.character(dat$CLOSE_DT)

# take CLOSE_DT as an example
dat$CLOSE_DT[1:6]

# in case as.Date() may give NA(s) in some locales
# setting the C locale
Sys.setlocale("LC_TIME", "C")

# --- Approach 1: create a new variable and transform --- #
idx <- which(is.na(dat$CLOSE_DT))
close_dt <- dat$CLOSE_DT[-idx]
close_dt <- paste0("01-",close_dt)
# Sys.setlocale("LC_TIME", "C")
close_dt <- as.Date(close_dt, "%d-%b-%Y")
## it works, and the result is date format!

# --- Approach 2: transform CLOSE_DT directly --- #
idx <- which(is.na(dat$CLOSE_DT))
dat$CLOSE_DT[-idx] <- paste0("01-",dat$CLOSE_DT[-idx])
dat$CLOSE_DT <- as.Date(dat$CLOSE_DT, "%d-%b-%Y")
## it works, and the result is date format!

## Conclusion: it is better to clean the data at first!
## Either replacing all NAs with meaningful and reasonable values or 
## extracting all the instances without NAs work!
## Make sure the dataset used for analysis is without NAs!

# --- Question 2: Merge data ---
library(readxl)
EntryCohor <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx",
                  sheet = "DHS_Case_Clients_2016EntryCohor")
Sys <- read_excel("DHS_CrossSystem.xlsx",
                  sheet = "SystemInvolvement_EC2016")

lenunique<-function(x) {
  length(unique(x))
}

# CASE_ID has duplicates in EntryCohor
check1 <- tapply(EntryCohor$CASE_ID, EntryCohor$CASE_ID, length)
check1

# CLIENT_ID has duplicates in EntryCohor
check2 <- tapply(EntryCohor$CLIENT_ID, EntryCohor$CLIENT_ID, length)
check2

# For a given CASE_ID, it may include multiple CLIENT_IDs in EntryCohor
check3 <- tapply(EntryCohor$CLIENT_ID, EntryCohor$CASE_ID, lenunique)
check3
table(check3)

# For example, CASE_ID:28892
# idx <- which(EntryCohor$CASE_ID==28892)
# EntryCohor$CLIENT_ID[idx]

# For a given CLIENT_ID, it may be invovled in mulitple CASE_ID in EntryCohor
# However, most of them (8774/8866) correspond to only one CASE_ID 
check4 <- tapply(EntryCohor$CASE_ID, EntryCohor$CLIENT_ID, lenunique)
check4
table(check4)
# check4
# 1    2    3 
# 8774   90    2 

# In System, CLIENT_ID is the key
check5 <- tapply(Sys$CLIENT_ID, Sys$CLIENT_ID, length)
check5
table(check5)
# check5
# 1 
# 8206 

# As a result, make CLIENT_ID be the key for EntryCohor, too
# For a given CLIENT_ID, the variables which can have more than one value:
# CASE_ID, ROLE, AGE_SRVC_ACPT_DT, CL_INLV_START, SRVC_ACPT_DT_MY
# CASE_TYPE_DESC, ACCEPT_REASON, CLOSE_DT, CASE_CLOSE_REASON
check <- tapply(EntryCohor$CASE_CLOSE_REASON, EntryCohor$CLIENT_ID, lenunique)
check
table(check)

pastex<-function(x) {
  n<-length(x)
  values<-""
  for (i in 1:n) {
    values<-paste(values, x[i], sep=",")
  }
  return(values)
}

# extract the following 4 people's instances c(688932, 688929, 689350, 690234)
# each instance in EntryCohor only involves four variables:
# CLIENT_ID, CASE_ID, CLOSE_DT, CASE_CLOSE_REASON
idx <- which(EntryCohor$CLIENT_ID==688932|
               EntryCohor$CLIENT_ID==688929|
               EntryCohor$CLIENT_ID==689350|
               EntryCohor$CLIENT_ID==690234)
EntryCohor_Sub <- EntryCohor[idx,c("CLIENT_ID", "CASE_ID", 
                                   "CLOSE_DT", "CASE_CLOSE_REASON")]
EntryCohor_Sub

idx <- which(Sys$CLIENT_ID==688932|
               Sys$CLIENT_ID==688929|
               Sys$CLIENT_ID==689350|
               Sys$CLIENT_ID==690234)
Sys_Sub <- Sys[idx,]
Sys_Sub

# combine multiple CASE_ID into one by CLIENT_ID in EntryCohor
case <- tapply(EntryCohor_Sub$CASE_ID, EntryCohor_Sub$CLIENT_ID, pastex)
case
case <- sub("^,", "", case) # remove , when it is at the start

# combine multiple CLOSE_DT into one by CLIENT_ID in EntryCohor
close <- tapply(EntryCohor_Sub$CLOSE_DT, EntryCohor_Sub$CLIENT_ID, pastex)
close
close <- sub("^,", "", close) # remove , when it is at the start

# combine multiple CASE_CLOSE_REASON into one by CLIENT_ID in EntryCohor
reason <- tapply(EntryCohor_Sub$CASE_CLOSE_REASON, EntryCohor_Sub$CLIENT_ID, pastex)
reason
reason <- sub("^,", "", reason)

x <- 1:dim(EntryCohor_Sub)[1]
idx <- tapply(x, EntryCohor_Sub$CLIENT_ID, min)
EntryCohor_Sub1 <- EntryCohor_Sub[idx,]
EntryCohor_Sub1$CASE_ID <- case
EntryCohor_Sub1$CLOSE_DT <- close
EntryCohor_Sub1$CASE_CLOSE_REASON <- reason
EntryCohor_Sub1

cbind(Sys_Sub$CLIENT_ID, EntryCohor_Sub1$CLIENT_ID)
bigdat<-cbind(Sys_Sub, EntryCohor_Sub1)
bigdat
write.table(bigdat, "FourClientsMerged.txt", sep="\t")