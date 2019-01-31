#calculate integrated volume of discharge throughout the event 
Qvol <- function(ev_data) {
  (sum(ev_data$MiBarCMS))*((15*60)*nrow(ev_data)) #multiply the total number of seconds by the sum of m^3/s 
}