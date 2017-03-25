dat <- read.csv("DHSMini.csv")

str(dat)
summary(dat)
dat[1:6,]

dat[which(dat$CaseID==32613),] #7
length(unique(dat$MH1[which(dat$CaseID==32613)])) #7 include NA
dat[which(dat$CaseID==31659),] #2
length(unique(dat$MH1[which(dat$CaseID==31659)])) #3 include NA
dat[which(dat$CaseID==32048),] #3
length(unique(dat$MH1[which(dat$CaseID==32048)])) #4 include NA

y <- AggregateByCase(dat$CaseID, dat$MH1)
str(y)
summary(y)

y[which(y$CaseID==32613),] #7
y[which(y$CaseID==31659),] #2
y[which(y$CaseID==32048),] #3

PlotByTime(y$CaseID, y$Count)