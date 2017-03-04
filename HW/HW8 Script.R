# --- from Week 8 Lecture Notes ---
# Let¡¯s work only with clients who are involved in 1 case
library(readxl)
dat <- read_excel("DHS_Case_Clients_2016EntryCohort.xlsx",
                  sheet = "DHS_Case_Clients_2016EntryCohor")

lenunique<-function(x){
  length(unique(x))
}

#identify these clients
numCases<-tapply(dat$CASE_ID, dat$CLIENT_ID, lenunique)
table(numCases) #check

listOfClient<-sort(unique(dat$CLIENT_ID))
multicaseID<-which(numCases>1)
multicaseclient<-listOfClient[multicaseID]
multicaseIdxCohort<-which(dat$CLIENT_ID %in% multicaseclient)
single<-dat[-multicaseIdxCohort,]
numCases<-tapply(single$CASE_ID, single$CLIENT_ID, lenunique)
table(numCases)  # check

# For a given CLIENT_ID, the variables which can have more than one value are
# CLOSE_DT, CASE_CLOSE_REASON. Let's deal with it. 
check <- tapply(single$CASE_CLOSE_REASON, single$CLIENT_ID, lenunique)
table(check) 


# --- Question 1 ---
pastex<-function(x){
  n<-length(x)
  values<-""
  for (i in 1:n) {
    values<-paste(values, x[i], sep=",")
  }
  values <- sub("^,", "", values) # remove the "," when it is at the start
  return(values)
}

# combine multiple CLOSE_DT into one by CLIENT_ID in EntryCohor
close <- tapply(single$CLOSE_DT, single$CLIENT_ID, pastex)
# do the same for CASE_CLOSE_REASON 
reason <- tapply(single$CASE_CLOSE_REASON, single$CLIENT_ID, pastex)
c(length(close),length(reason))  #check

# now we want to retain just 1 row of observation per client
rowno <- 1:dim(single)[1]
# let's get the row number corresponding to the first time this client is observed in the data
idx <- tapply(rowno, single$CLIENT_ID, min) 
Cohort <- single[idx,] # 8774
Cohort$CLOSE_DT <- close
Cohort$CASE_CLOSE_REASON <- reason
Cohort[1:2,] 
Cohort <- Cohort[order(Cohort$CLIENT_ID),] 


Sys <- read_excel("DHS_CrossSystem.xlsx",
                  sheet = "SystemInvolvement_EC2016")

# remove client involved in more than one case
Sys_Sub <- Sys[-which(Sys$CLIENT_ID %in% multicaseclient),] #8118
dim(Sys_Sub)  #8118
Sys_Sub  <- Sys_Sub[order(Sys_Sub $CLIENT_ID),] 

# # 690 client that is in Cohort, not in Sys_Sub 
onlyCohort<- which(!(Cohort$CLIENT_ID %in% Sys_Sub$CLIENT_ID))
length(onlyCohort)  


# 34 client that is in Sys_Sub, not in Cohort)
onlySys_Sub<-which(!(Sys_Sub$CLIENT_ID %in% Cohort$CLIENT_ID))
length(onlySys_Sub)

# remove the ones that don't exist in the other database and merge the two databases
Cohort1 <- Cohort[-onlyCohort,]  
Sys_Sub1 <- Sys_Sub[-onlySys_Sub,] 
Merg<-cbind(Sys_Sub1, Cohort1)  # inner join

#check that we did it right
any(Merg[,1]!=Merg[,45]) 
Merg<-Merg[,-1] # yes we did ! remove first CLIENT_ID column, don't need it anymore

# output and save
write.table(Merg, "ClientsMerged.txt", sep="\t")

# use merge function
# inner join (intersection only)
ans_Inner <- merge(Sys_Sub, Cohort, all=FALSE) # 8084
# outer join
ans_outer <- merge(Sys_Sub, Cohort, all=TRUE) # 8808=8084+690+34
# left outer join (Sys_Sub)
ans_Left <- merge(Sys_Sub, Cohort, all.x=TRUE) # 8118
# right outer join (Cohort)
ans_Right <- merge(Sys_Sub, Cohort, all.y=TRUE) # 8774

## checking the difference
diff<-which(ans_Left$CLIENT_ID!=Merg$CLIENT_ID)
idx<-diff[1]
rbind(ans_Left[idx-1,], Merg[idx-1,])
rbind(ans_Left[idx,], Merg[idx,])


# --- Question 2 ---
ClientsMerged <- read.delim("ClientsMerged.txt")
ClientsMerged <- read.table("ClientsMerged.txt")
str(ClientsMerged)
ClientsMerged[1:6,]

# CrossID?
new<-c("ACHA1", "ACHA2", "AIUC1", "AIUC2", "AIUP1", "AIUP2")
new<-c(new, "CYFK1", "CYFK2", "CYFP1", "CYFP2", "DA1", "DA2")
new<-c(new, "DPWF1", "DPWF2", "DPWG1", "DPWG2", "DPWS1", "DPWS2")
new<-c(new, "DPWT1", "DPWT2", "EI1", "EI2", "FSC1", "FSC2")
new<-c(new, "HA1", "HA2", "HH1", "HH2", "IL1", "IL2", "JPO1", "JPO2")
new<-c(new, "JPOC1", "JPOC2", "JPOK1", "JPOK2","JPOP1", "JPOP2")
new<-c(new, "MH1", "MH2", "ID1", "ID2","CaseID", "ClientID", "Role", "DOB",
       "AgeCYF")
new<-c(new, "Gender", "Race", "CaseStart", "ClientMin", "ClientType","AcceptDate",
       "CaseType", "AcceptReason", "CloseDate", "CloseReason")
names(ClientsMerged) <- new
str(ClientsMerged)


# --- Question 3 ---
Sys.setlocale("LC_TIME", "C")
changeToAsDate<-function(x){
  datestr<-as.character(x)
  idx<-which(is.na(datestr))
  datestr[-idx]<-paste0("01-",datestr[-idx])
  datestr<-as.Date(datestr, "%d-%b-%Y")
  return(datestr)
}
# For example, CloseDate
# ClientsMerged$CloseDate <- changeToAsDate(ClientsMerged$CloseDate)

datelist <- c("ACHA1", "ACHA2", "AIUC1", "AIUC2", "AIUP1", "AIUP2",
              "CYFK1", "CYFK2", "CYFP1", "CYFP2", "DA1", "DA2",
              "DPWF1", "DPWF2", "DPWG1", "DPWG2", "DPWS1", "DPWS2",
              "DPWT1", "DPWT2", "EI1", "EI2", "FSC1", "FSC2",
              "HA1", "HA2", "HH1", "HH2", "IL1", "IL2", "JPO1", "JPO2",
              "JPOC1", "JPOC2", "JPOK1", "JPOK2","JPOP1", "JPOP2",
              "MH1", "MH2", "ID1", "ID2",
              "CaseStart", "ClientMin",
              "AcceptDate","CloseDate")

for(i in datelist){
  ClientsMerged[,i] <- changeToAsDate(ClientsMerged[,i])
}
str(ClientsMerged) # check the result


# --- Question 4 ---
# we can write a function to do this in a much briefer way but this long way works too

# Gender
summary(ClientsMerged$Gender)
ClientsMerged$Gender <- as.character(ClientsMerged$Gender)
ClientsMerged$Gender[which(ClientsMerged$Gender=="Female")] <- "F"
ClientsMerged$Gender[which(ClientsMerged$Gender=="Male")] <- "M"
ClientsMerged$Gender[which(ClientsMerged$Gender=="Unknown")] <- "U"

ClientsMerged$Gender <- as.factor(ClientsMerged$Gender)
summary(ClientsMerged$Gender)

# Role
summary(ClientsMerged$Role)
ClientsMerged$Role <- as.character(ClientsMerged$Role)
ClientsMerged$Role[which(ClientsMerged$Role=="Child")] <- "C"
ClientsMerged$Role[which(ClientsMerged$Role=="Father")] <- "F"
ClientsMerged$Role[which(ClientsMerged$Role=="Mother")] <- "M"
ClientsMerged$Role[which(ClientsMerged$Role=="Other")] <- "O"

ClientsMerged$Role <- as.factor(ClientsMerged$Role)
summary(ClientsMerged$Role)

# CaseType
summary(ClientsMerged$CaseType)
ClientsMerged$CaseType <- as.character(ClientsMerged$CaseType)
ClientsMerged$CaseType[which(ClientsMerged$CaseType=="Family Services")] <- "FS"
ClientsMerged$CaseType[which(ClientsMerged$CaseType=="Resumption")] <- "R"

ClientsMerged$CaseType <- as.factor(ClientsMerged$CaseType)
summary(ClientsMerged$CaseType)

# ClientType
summary(ClientsMerged$ClientType)
ClientsMerged$ClientType <- as.character(ClientsMerged$ClientType)
ClientsMerged$ClientType[
  which(ClientsMerged$ClientType=="CYF Active")] <- "A"
ClientsMerged$ClientType[
  which(ClientsMerged$ClientType=="CYF Active/IL Open Referral")] <- "OR"

ClientsMerged$ClientType <- as.factor(ClientsMerged$ClientType)
summary(ClientsMerged$ClientType)

# Race
summary(ClientsMerged$Race)
ClientsMerged$Race <- as.character(ClientsMerged$Race)

# American Indian or Alaska Native
ClientsMerged$Race[
  grep("American Indian or Alaska Native", ClientsMerged$Race)] <- "AI_AN"
# Black or African American
ClientsMerged$Race[
  grep("Black or African American", ClientsMerged$Race)] <- "B"
# Chinese
# Indian
# Filipino
# Vietnamese
# Asian
ClientsMerged$Race[
  grep("Chinese", ClientsMerged$Race)] <- "A"
ClientsMerged$Race[
  grep("Indian", ClientsMerged$Race)] <- "A"
ClientsMerged$Race[
  grep("Filipino", ClientsMerged$Race)] <- "A"
ClientsMerged$Race[
  grep("Vietnamese", ClientsMerged$Race)] <- "A"
ClientsMerged$Race[
  grep("Asian", ClientsMerged$Race)] <- "A"
# Guamanian or Chamorro
# Other Race
ClientsMerged$Race[
  grep("Guamanian or Chamorro", ClientsMerged$Race)] <- "O"
ClientsMerged$Race[
  grep("Other Race", ClientsMerged$Race)] <- "O"
# Unknown
ClientsMerged$Race[
  which(ClientsMerged$Race=="Unknown")] <- "U"
# White
ClientsMerged$Race[
  which(ClientsMerged$Race=="White")] <- "W"

ClientsMerged$Race <- as.factor(ClientsMerged$Race)
summary(ClientsMerged$Race)
