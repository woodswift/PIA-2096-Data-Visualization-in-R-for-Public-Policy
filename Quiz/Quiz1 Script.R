dataset <- read.csv("FriendsOfLibraryBookSales.csv")
summary(dataset)

# find NA in month
naMMIdx <- which(is.na(dataset$month))

# find NA in year
naYYIdx <- which(is.na(dataset$year))

# NA Idx
Idx <- c(naMMIdx, naYYIdx)

# get uniques
Idx <- unique(Idx)

# get records
useful <- dataset[-Idx,]

summary(useful)

# histogram
hist(useful$sales, col="blue", 
     xlab='Number of books sold per month', 
     main='Sales (time period known)')

# create variable "quarter"
dataset$quarter <- 0
dataset$quarter <- ifelse(dataset$month>=1 & 
                            dataset$month<=3, 1, dataset$quarter)
dataset$quarter <- ifelse(dataset$month>=4 & 
                            dataset$month<=6, 2, dataset$quarter)
dataset$quarter <- ifelse(dataset$month>=7 & 
                            dataset$month<=9, 3, dataset$quarter)
dataset$quarter <- ifelse(dataset$month>=10 & 
                            dataset$month<=12, 4, dataset$quarter)

# another way
dataset$quarter <- NA
dataset$quarter <- ifelse(dataset$month>=1 & 
                            dataset$month<=3, 1, NA)
naIdx <- which(is.na(dataset$quarter))
dataset$quarter[naIdx] <- ifelse(dataset$month[naIdx]>=4 & 
                            dataset$month[naIdx]<=6, 2, NA)
naIdx <- which(is.na(dataset$quarter))
dataset$quarter[naIdx] <- ifelse(dataset$month[naIdx]>=7 & 
                                   dataset$month[naIdx]<=9, 3, NA)
naIdx <- which(is.na(dataset$quarter))
dataset$quarter[naIdx] <- ifelse(dataset$month[naIdx]>=10 & 
                                   dataset$month[naIdx]<=12, 4, NA)
