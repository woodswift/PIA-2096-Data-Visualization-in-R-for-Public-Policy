AggregateByCase <- function(group,x){
  # group represents CaseID
  # x represents MH1
  
  group <- dat$CaseID
  x <- dat$MH1
  
  # extract NA index in MH1
  naIdx <- which(is.na(x)==TRUE)
  
  # compute # of non NAs in MH1 for every CaseID
  numCase <- tapply(x[-naIdx], group[-naIdx], length)
  
  # store in data frame
  stat <- data.frame(CaseID=names(numCase), Count=as.vector(numCase))
  rownames(stat) <- 1:length(numCase)
  
  return(stat)
}

lenunique <- function(x){
  length(unique(x))
}