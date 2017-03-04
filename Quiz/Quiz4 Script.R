# Quiz 4 -- help these guys fix their code. 
# How many can you help?

#1. Joe have a vector of family names, and 
# a corresponding vector of
# the ages of all the children in the family.
# Joe wants to calculate the mean ages of the Lim family.
# (Result should be : [1] 4.333333)

family<-c("Smith", "Lim", "Roy")
ages<-c("1,8", "4,4,5", "2, 9")
x<-strsplit(ages, split=",")
mean(x[[2]])

# answer:
family<-c("Smith", "Lim", "Roy")
ages<-c("1,8", "4,4,5", "2, 9")
x<-strsplit(ages, split=",")
mean(as.numeric(x[[2]]))


#2. Mary has vectors of names of the kids in each family
# e.g : the kids in the Smith family are Yuni and Lani. 
# Mary wants to know if the same kid names are used across families
# (Result should be: 
# sameName
# FALSE  TRUE 
# 5     2 
# )

kidsSmith<-c("Yuni", "Lani")
kidsLim<-c("Nancy", "Roger", "Sue")
kidsRoy<-c("Lani", "Sue")

sameName<-c(kidsSmith == kidsLim, kidsLim == kidsRoy, kidsRoy == kidsSmith)
length(sameName)

# answer:
kidsSmith<-c("Yuni", "Lani")
kidsLim<-c("Nancy", "Roger", "Sue")
kidsRoy<-c("Lani", "Sue")
sameName<-c(kidsSmith%in%kidsLim, kidsLim%in%kidsRoy, kidsRoy%in%kidsSmith)
table(sameName)


#3. John has a list of family incomes (in thousands of $) and number of services each family gets
# He wants to compute the largest and smallest family income for each number of services. 
# Help him fix his for-loop.
# Result should be:
#[1] "0 services, income: 13 - 19"
#[1] "1 services, income: 14 - 23"
#[1] "2 services, income: 15 - 24"
#[1] "3 services, income: 17 - 28"
#[1] "5 services, income: 22 - 22"
dat<-read.csv("Quiz4.csv", header=TRUE)
numservice<-unique(dat$service)
for (i in 1:numservice) {
  paste(i, "services, income:", min(dat$income[i]), max(dat$income[i]))
}

# answer:
dat<-read.csv("Quiz4.csv", header=TRUE)
numservice<-unique(dat$service)
numservice<-sort(numservice)
for (i in numservice) {
  str<-paste(i, "services, income:", 
             min(dat$income[dat$service==i]),
             " ",
             max(dat$income[dat$service==i]))
  print(str)
}


#4. Lily wants to create a general function to plot means by group.
# she wants to use it to plot a line graph that describes average income (y)
# as a function of the number of services used by a family (x). 

plotmean <-function (x1, x2, graphtype="p") {
  x<-tapply(x1, x2, mean)
  y<-unique(x1)
  plot(x,y, type=graphtype)
}

# answer:
plotmean <-function (x1, x2, graphtype="p") {
  x<-tapply(x1, x2, mean)
  y<-names(x)
  plot(y, x, type=graphtype)
}
plotmean(dat$income, dat$service, graphtype="l")