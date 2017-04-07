rm(list = ls())
airquality <- airquality

### 3 pts.
library(dplyr)
dat <- airquality %>% 
  mutate(windcategory = round((Wind+3)/5, 0)) %>% 
  group_by(windcategory) %>% 
  summarise(avgWindMph = round(mean(Wind),2), N = n())
  
  
### extra credit
calDataQuality <- function (N, Ozone){
  if (N < 10){
    return("Too few observations")
  } else {
    count <- 0
    for (i in 1:length(Ozone)){
      if (is.na(Ozone[i])){
        count <- count + 1
      }
    }
    return(paste0("Ozone info: ",round((count / N * 100), 2), "% missing"))
  }
}

dat <- airquality %>% 
  mutate(windcategory = round((Wind+3)/5, 0)) %>% 
  group_by(windcategory) %>% 
  summarise(avgWindMph = round(mean(Wind),2), N = n(), dataQuality = calDataQuality(N, Ozone))



