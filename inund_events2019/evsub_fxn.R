#function needs to query the event_pre dataframe to get the beginning and end of each event 
#subset discharge data to the length of each flooding event

evsub <- function(d_start, d_end) {
  library(lubridate)
  library(dplyr)
  library(here)
  #get data
  mb <- read.csv(here::here("data", "mich_bar_cms_all.csv"), stringsAsFactors = FALSE)
  mb$Date <- as.POSIXct(mb$Date, format = "%m/%d/%Y %H:%M")
  mb %>% 
    dplyr::select(Date, MiBarCMS) %>%
    dplyr::filter(Date >= as.POSIXct(d_start) & Date <= as.POSIXct(d_end))
}
