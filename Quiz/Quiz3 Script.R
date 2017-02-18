family <- c("Smith", "Lim")
kidnames <- c("Yuni, Lani", "Nancy, Roger, Sue")

# Step 1
pastenames <- function(firstlist, last){
  # firstlist is a string contain all first names
  # last is a value representing family names
  
  # split and get a list
  firstlist1 <- strsplit(firstlist, ",")
  # transform a list into a vector
  firstlist2 <- unlist(firstlist1)
  
  ans <- NULL
  for(i in firstlist2){
    tmp <- paste0(i, " ", last)
    ans <- c(ans, tmp)
  }
  return(ans)
}

# Step 2
fullnames <- NULL
for (i in 1:length(family)){
  # assumption: kidnames has the same length as family
  tmp <- pastenames(kidnames[i], family[i])
  fullnames <- c(fullnames, tmp)
}

# returns string w/o leading whitespace
trim.leading <- function (x){
  sub("^\\s+", "", x)
}

fullnames <- trim.leading(fullnames)

fullnames
# [1] "Yuni Smith" "Lani Smith" "Nancy Lim"  "Roger Lim"  "Sue Lim"   