# load dataset
dat <- read.csv("FractionEnglishLearner.csv")

dat[1:10,]
str(dat)
summary(dat)

# scatterplot
goodLearner <- ifelse(dat$englishLearner>0.5, "red", "dark gray")
plot(jitter(dat$grade), jitter(dat$classSize), 
     cex=5*dat$englishLearner, # enlarge the circle by 5 times
     col=goodLearner, # distinguish two groups
     xlab="Grade", ylab="Class Size", 
     main="Fraction of English Learn across Classrooms")

# calculate the mean
gradeList <- unique(dat$grade) # get the unique record in grade
gradeList <- sort(gradeList) # sort in ascending order
meanESL <- NULL # initialize meanESL
for(g in gradeList){
  m <- mean(dat$englishLearner[dat$grade==g]) # locate records and calculate the mean
  m <- round(m,2) # round to 2 digits
  meanESL <- c(meanESL, m) # store the result
}
meanESL