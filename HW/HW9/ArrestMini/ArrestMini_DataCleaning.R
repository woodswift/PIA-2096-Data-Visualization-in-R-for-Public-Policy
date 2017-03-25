AggregateByCase <- function(group,x){
  # group represents ARRESTTIME
  # x represents COUNCIL_DISTRICT
  
  # extract date from ARRESTTIME
  arresttime <- as.character(group)
  time_date <- sub("T", " ", arresttime)
  
  library(lubridate)
  time_date_pos <- as.POSIXct(time_date)
  
  dateNew <- date(time_date_pos)
  
  # extract NA index in MH1
  naIdx <- which(is.na(x)==TRUE)
  
  # compute # of non NAs in COUNCIL_DISTRICT for every ARRESTTIME
  numDate <- tapply(x[-naIdx], dateNew[-naIdx], lenunique)
  
  # store in data frame
  stat <- data.frame(Date=names(numDate), Count=as.vector(numDate))
  rownames(stat) <- 1:length(numDate)
  
  # str(stat)
  # summary(stat)
  # stat[1:100,]
  
  return(stat)
}


lenunique <- function(x){
  length(unique(x))
}