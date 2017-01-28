# load dataset
dat=read.csv("FriendsOfLibraryBookSales.csv")

# view dataset briefly
dat[1:6,]
str(dat)
summary(dat)

# create vector to store each column
sales<-dat$sales
month<-dat$month
year<-dat$year

# 1.1 check if there are any data for September 2011.
# The output should be a single boolean.
sept2011check <- any(month==9 & year==2011)
sept2011check # TRUE

# 1.2 output a vector of boolean that indicate whether a row 
# corresponds to data for Sept 2011.
# Save this into a variable called sept2011.
# then count the number of T, F, and NA with table().
# Hint: search the table() help file for hints
# on the useNA parameter.
sept2011 <- month==9 & year==2011
table(sept2011)
table(sept2011, useNA = "no")
table(sept2011, useNA = "ifany") # F:2922, T:32, NA:46
table(sept2011, useNA = "always") # F:2922, T:32, NA:46

# 1.3 output a vector with the indices corresponding to data for Sept 2011.
# save this into a vector called sept2011idx.
# use this vector to calculate the mean sales for Sept 2011.
sept2011idx <- which(month==9 & year==2011)
mean(sales[sept2011idx]) #327.1562

# 1.4 contrast what you did in 1.3 (with sept2011idx) with 
# calculating the mean sales for Sept
# 2011 using the vector of boolean (sept2011) from 1.2.
# is the output different? why/why not?
mean(sales[sept2011]) #NA #sales[NA] will return NA

# 1.5 Copy sales into a new variable sales2.
# Look up the function sort and sort sales2 in ascending order.
sales2 <- sales
# ascending order
sales2 <- sort(sales2)
# Check your work by comparing the first 10 elements of sales 
# with the first 10 elements of sales2.
sales[1:10]
sales2[1:10]

# Can you use sept2011idx on sales2 to get sales data for Sept 2011? Why/why not?
mean(sales[sept2011idx]) #327.1562
mean(sales2[sept2011idx]) #337.8438 #indexing is not the same after sorting

# filter the sales data so they
# can focus on the past 10 summers (June-August from 2007 onward).

#2.a Alejandro wrote the following code:
summer<-(month>=6 & month<=8)
recent <- (year>=2017-10)
summersales<-sales[summer & recent]

# 2.a modification
# extract index which meets "month" condition
summer <- which(month>=6 & month<=8)
# extract index which meets "year" condition
recent <- which(year>=2017-10)
# extract the common index between the above two indices
commonIdx <- intersect(summer, recent)
summersales <- sales[commonIdx]

# result verification
table(year[commonIdx])
table(month[commonIdx]) # 6:153, 7:151, 8:199

#2.b Jiangwei wrote the following code:
idx <- which(month<6 | month>9 | year<2007)
summersales<-sales[-idx]

# 2.b modification
# except NAs, extract index which voliates the "month" and "year" conditions
idx <- which(month<6 | month>8 | year<2007)
# find index which is NA in month and year, and then union them
naidx <- union(which(is.na(month)), which(is.na(year)))
# union idx and naidx to form the index not meeting the conditions
idx <- union(idx, naidx)
summersales<-sales[-idx]

# result verification
table(year[-idx])
table(month[-idx]) # 6:153, 7:151, 8:199

#2.c Savion wrote the following code:
recent<-which(!(year<2007))
recentmonth<-month[recent]
recentsummermonth<-recentmonth[(month>=6 & month<=8)]
summersales<-sales[recentsummermonth]

# 2.c modification (also the answer to 2.e)
# extract idx which meets all the "month" and "year" conditions
recent <- which(!(year<2007) & month>=6 & month<=8)
summersales<-sales[recent]

# result verification
table(year[recent])
table(month[recent]) # 6:153, 7:151, 8:199

# Here's a version of the loop I wrote in class to compute average monthly sales 
# volume for each year
year_list <- unique(year)
for (y in year_list) {
  m=mean(sales[year==y], na.rm = TRUE)
  print(paste("Year", y, "Mean monthly sales $", m))
}

# 3.a this code has 2 problems in dealing with NA.Identify and fix them
year_list <- unique(year) # note that year_list contains NA
na_year_idx <- which(is.na(year_list)) # find the index of NA in year_list
year_list <- year_list[-na_year_idx] # year_list does not contain NA any more
for (y in year_list) {
  m <- mean(sales[which(year==y)], na.rm = TRUE) # which(year==y) returns the corresponding indices
  print(paste("Year", y, "Mean monthly sales $", m))
}

# 3.b now improve the code in 2 ways: sort the year so it outputs 
# the earliest years first.
# and round mean monthly sales to the nearest cents.
year_list <- unique(year) # note that year_list contains NA
na_year_idx <- which(is.na(year_list)) # find the index of NA in year_list
year_list <- year_list[-na_year_idx] # year_list does not contain NA any more
year_list <- sort(year_list) # sort year_list in ascending order
for (y in year_list) {
  m <- mean(sales[which(year==y)], na.rm = TRUE) # which(year==y) returns the corresponding indices
  m <- round(m,2) # round mean monthly sales to the nearest cents
  print(paste("Year", y, "Mean monthly sales $", m))
}

# 3.c modify the loop so that it stores m in an vector called mean_msales
# instead of printing the output. Hint: Add this line of code before your loop:
mean_msales <-NULL
year_list <- unique(year) # note that year_list contains NA
na_year_idx <- which(is.na(year_list)) # find the index of NA in year_list
year_list <- year_list[-na_year_idx] # year_list does not contain NA any more
year_list <- sort(year_list) # sort year_list in ascending order
for (y in year_list) {
  m <- mean(sales[which(year==y)], na.rm = TRUE) # which(year==y) returns the corresponding indices
  m <- round(m,2) # round mean monthly sales to the nearest cents
  print(paste("Year", y, "Mean monthly sales $", m))
  mean_msales <- c(mean_msales, m)
}
mean_msales

#3.d HARDER: modify the loop to calculate the number of (unique) months for which we have
# data every year
# store the result in a vector called nmonths.
nmonths <-NULL
year_list <- unique(year) # note that year_list contains NA
na_year_idx <- which(is.na(year_list)) # find the index of NA in year_list
year_list <- year_list[-na_year_idx] # year_list does not contain NA any more
year_list <- sort(year_list) # sort year_list in ascending order
for (y in year_list) {
  month_list <- month[which(year==y)] # month_list may have NA and repeatation now
  month_list <- unique(month_list) # remove repeatation
  if(any(is.na(month_list)==TRUE)){
    # NA exists
    month_list <- month_list[-which(is.na(month_list))] # remove NA
  }
  n <- length(month_list)
  nmonths <- c(nmonths, n)
  print(paste("Year", y, "the number of unique months:", n))
}
nmonths
