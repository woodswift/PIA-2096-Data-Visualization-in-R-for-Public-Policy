# returns string w/o leading whitespace
trim.leading <- function (x){
  sub("^\\s+", "", x)
}


# Q1
# 1A. Understand how "[0-9]+" works in the code below.
gregexpr("[0-9]+", c("5602 theft", "of property", " 703 battery", 
                     "victim age below 21", "13(a)", "703 battery", "13(a)"))
# [0-9]: any single character in the range 1-9
# a+: one or more of a

# Note: Splitting the offenses in crimine data by "/" will give you a list of
# strings similar to the one above

strings <- c("5602 theft", "of property", " 703 battery", 
             "victim age below 21", "13(a)", "703 battery", "13(a)")
matches<-gregexpr("[0-9]+", strings)
matches
regmatches(strings, matches)
# "5602 theft": 5602; index: 1
# "of property": none; index: -1
# " 703 battery": 703; index: 2
# "victim age below 21": 21; index: 18
# "13(a)": 13; index: 1
# "703 battery": 703; index: 1
# "13(a)": 13; index: 1


# 1B. We¡¯re really only interested in the offense code, 
# which you can identify as the number that
# starts as the 1st or 2nd character 
# in the string (¡°5602¡±, ¡°703¡±, ¡°13¡±, or ¡°13(a)¡± but not ¡°21¡±).
# Use regexpr(), which(), and regmatches to output a vector of offense code.

strings <- trim.leading(strings)

index <- regexpr("[0-9]+", strings)
index <- which(index==1)

matches<-gregexpr("[0-9]+", strings)
matches

ans <- regmatches(strings, matches)[index]
ans <- sapply(ans, "[[", 1)
# ans <- unlist(ans) # ok to use it, too


# 1C. Once you output the vector of offense code, 
# compute the frequency of offenses using
# table() and sort the output from most to least frequent. Call it counts1.
counts1 <- sort(table(ans), decreasing=TRUE)
counts1


# Q2
dat <- read.csv("ArrestData.csv")

str(dat$OFFENSES)
summary(dat$OFFENSES)
dat$OFFENSES[1:6]

offense <- dat$OFFENSE
offense <- as.character(offense) #4805

# 2A. Split the variable offenses by "/"
offense1 <- strsplit(offense, split="/") #4805
offense2 <- unlist(offense1) #12231

# 2B. Produce a table of frequency of offenses.
# Call it counts2. It should have 212 elements.

# remove string w/o leading whitespace
offense2 <- trim.leading(offense2)

# return index of string which starts with number
# regexpr: gives the starting position of the first match
index <- regexpr("[0-9]+", offense2)
index <- which(index==1) #11477

# gregexpr: returns a list of the same length as text 
# each element of which is of the same form as the return value 
matches<-gregexpr("[0-9]+", offense2) #12231
matches

ans <- regmatches(offense2, matches)[index] #11477

# get every element of the list by apply a function
ans <- sapply(ans, "[[", 1) #11477

# cannot use unlist() here directly, lapply() first and then unlist()
# ans <- lapply(ans, "[[", 1)
# ans <- unlist(ans)

# get frequency and sort
counts2 <- sort(table(ans), decreasing=TRUE) #212
counts2[1:10]


# 2C
length_v <- sapply(ans, nchar)
ans1 <- ans
idx_2 <- which(length_v==2) #string length 2: 2214
idx_3 <- which(length_v==3) #string length 3: 612
idx_4 <- which(length_v==4) #string length 4: 8651
ans1[idx_2] <- "13(a)" # 13(a) remains as 13(a)
ans1[idx_3]<-substr(ans[idx_3], 1,1) # the first of the 3 number codes
ans1[idx_4]<-substr(ans[idx_4], 1,2) # the first 2 digits of the 4 number codes
counts3<-sort(table(ans1), decreasing=TRUE) #42
counts3[1:10]


# Q3
dat <- read.csv("ArrestData.csv")
arresttime <- dat$ARRESTTIME
arresttime <- as.character(arresttime)

time_date <- sub("T", " ", arresttime)
time_date

library(lubridate)
time_date_pos <- as.POSIXct(time_date)

month <- month(time_date_pos)
days <- wday(time_date_pos)
hour <- hour(time_date_pos)

# attach to the data frame
dat <- data.frame(dat, month=month, days=days, hour=hour)

table(month) # 10
table(days) # 3: Tuesday
table(hour) # 16: 4:00pm

sort(table(month))
sort(table(days))
sort(table(hour))
