dat <- read.csv("ArrestMini.csv")

str(dat)
summary(dat) # min of COUNCIL_DISTRICT is 1
dat[1:6,]

y <- AggregateByCase(dat$ARRESTTIME, dat$COUNCIL_DISTRICT)
str(y)
summary(y)

y[which(y$Date=="2016-09-12"),] # 8 council

PlotByTime(y$Date, y$Count)