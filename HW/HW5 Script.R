# Q1
# 1a
dat<-read.csv("fractionEnglishLearner.csv")
color<-ifelse(dat$englishLearner>0.5, "red", "dark gray")
plot(jitter(dat$grade), jitter(dat$classSize), cex=dat$englishLearner*5, 
     col=color, lwd=2, ylab="class size", xlab="grade level",
     main="fraction of English learner across classrooms")

g<-1:12
meanESL<-rep(NA,12)
for (i in g){
  esl_grade<-dat$englishLearner[which(dat$grade==i)]
  meanESL[i]<-round(mean(esl_grade),2)
}
meanESL

# 1b
lineGraphOfGroupMeans <- function(x, group, existingPlot=FALSE){
  # Call the function meanByGroup() which you did in class on 2/2/2017
  lineValue <- meanByGroup(x, group)
  g <- sort(unique(group))
  if(existingPlot){
    # If existingPlot is TRUE, 
    # add a line graph of group averages to an existing plot
    lines(g, lineValue) 
  }
  else{
    # If existingPlot if FALSE, 
    # draw a line graph of group averages on a new plot
    plot(g, lineValue, xlab="group",ylab="value")
    lines(g, lineValue)
  }
}

meanByGroup <- function(x, y){
  # x: values; y: group
  yList <- unique(y)
  yList <- sort(yList)
  mBG <- NULL
  for(i in yList){
    m <- mean(x[y==i], na.rm=TRUE)
    m <- round(m, 2)
    mBG <- c(mBG, m)
  }
  return(mBG)
}

lineGraphOfGroupMeans(dat$classSize, dat$grade, existingPlot=FALSE)
lineGraphOfGroupMeans(dat$classSize, dat$grade, existingPlot=TRUE)

# 1c
lineGraphOfGroupMeans <- function(x, group, 
                                  xtitle="group", ytitle="value", 
                                  existingPlot=FALSE){
  # Call the function meanByGroup() which you did in class on 2/2/2017
  lineValue <- meanByGroup(x, group)
  g <- sort(unique(group))
  if(existingPlot){
    # If existingPlot is TRUE, 
    # add a line graph of group averages to an existing plot
    lines(g, lineValue) 
  }
  else{
    # If existingPlot if FALSE, 
    # draw a line graph of group averages on a new plot
    plot(g, lineValue, xlab=xtitle, ylab=ytitle)
    lines(g, lineValue)
  }
}

lineGraphOfGroupMeans(dat$classSize, dat$grade, 
                      "grade level", "class size", existingPlot=FALSE)
lineGraphOfGroupMeans(dat$classSize, dat$grade, 
                      "grade level", "class size", existingPlot=TRUE)

# Q2
dataset <- read.csv("FriendsOfLibraryBookSales.csv")
summary(dataset)

no_na_dataset <- na.omit(dataset)
summary(no_na_dataset) # 2434 rows

any.is.na<-function(x) {any(is.na(x))}

na.omit2<- function(d) {
  remove<-apply(d, 1, any.is.na)
  return(d[which(remove==FALSE),])
}

no_na_dataset2 <- na.omit2(dataset)
summary(no_na_dataset2) # 2434 rows
