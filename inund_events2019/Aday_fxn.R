#calculate area*days throughout each event
Aday <- function(ev_data) {
  (sum(ev_data$area_km2)) #sum km^2 over the event to get area-days for each event
}