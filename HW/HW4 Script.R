library(jsonlite)

# define the city names
city <- c("Pittsburgh","Philadelphia","Boston","Seattle","Atlanta","New Orleans")

for (address in city){
  # Connecting to the Google Maps to return coordinates given a location name
  root <- "http://maps.google.com/maps/api/geocode/json?"
  url <- paste0(root,"address=",address,"&sensor=false")
  raw.data <- readLines(url)
  dat <- fromJSON(raw.data)
  lat <- dat$results$geometry$location$lat
  long <- dat$results$geometry$location$lng
  
  # Connecting to the Google Maps API and downloading maps given a coordinate
  base <- "http://maps.googleapis.com/maps/api/staticmap?center="
  zoom <- 13
  maptype <- "hybrid"
  suffix <- "&size=800x800&sensor=false&format=png"
  target <- paste0(base,lat,",",long,"&zoom=",zoom,"&maptype=",maptype,suffix)
  filename <- paste0(address,".png")
  download.file(target,filename,mode="wb")
}