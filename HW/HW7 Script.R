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
dat$CLOSE_DT[-idx] <- as.Date(dat$CLOSE_DT[-idx], "%d-%b-%Y")
## it also works but the result is numeric format...

# Compared with SRVC_ACPT_DT_MY where no NA(s) exist
dat$SRVC_ACPT_DT_MY[1:6]
dat$SRVC_ACPT_DT_MY <- paste0("01-",dat$SRVC_ACPT_DT_MY)
dat$SRVC_ACPT_DT_MY <- as.Date(dat$SRVC_ACPT_DT_MY, "%d-%b-%Y")
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

# extract the following 4 people's instances c(688932, 688929, 689350, 690234)
idx <- which(EntryCohor$CLIENT_ID==688932|
               EntryCohor$CLIENT_ID==688929|
               EntryCohor$CLIENT_ID==689350|
               EntryCohor$CLIENT_ID==690234)
EntryCohor_Sub <- EntryCohor[idx,]
EntryCohor_Sub

idx <- which(Sys$CLIENT_ID==688932|
               Sys$CLIENT_ID==688929|
               Sys$CLIENT_ID==689350|
               Sys$CLIENT_ID==690234)
Sys_Sub <- Sys[idx,]
Sys_Sub

# natural join, which is default
m1 <- merge(EntryCohor_Sub, Sys_Sub, all=FALSE)

# left outer join
m2 <- merge(EntryCohor_Sub, Sys_Sub, all.x=TRUE)

# right outer join
m3 <- merge(EntryCohor_Sub, Sys_Sub, all.y=TRUE)

# outer join
m4 <- merge(EntryCohor_Sub, Sys_Sub, all=TRUE)