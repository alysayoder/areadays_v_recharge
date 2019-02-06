#function needs to query the event_pre dataframe to get the beginning and end of each event 
#subset inundation area data to the length of each flooding event

evsub <- function(d_start, d_end) {
  library(lubridate)
  library(dplyr)
  library(here)
  #get data
  inundata <- read.csv(here::here("data", "Whipple_Hydrospatial_PostRestoration_wy2013-2017.csv"), stringsAsFactors = FALSE)
  inundata <- inundata[,-c(2:3,5:6)]
  inundata$dt <- as.POSIXct(inundata$dt, format = "%m/%d/%Y")
  inundata %>% 
    dplyr::select(dt, area_km2) %>%
    dplyr::filter(dt >= as.POSIXct(d_start) & dt <= as.POSIXct(d_end))
}
