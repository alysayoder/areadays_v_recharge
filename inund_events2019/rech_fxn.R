# Function that takes inputs of groundwater levels at the start and end of a recharge event and returns
# volume of groundwater recharged throughout the event. 

# import water levels at the beginning and end of the event
# input files' columns should be ordered: lat, long, level
# the 'level' column should be in meters of groundwater elevation relative to mean sea level

# calculate recharge volume w/specific yield range
rechvol <- function(begin, end, plot_title, print = TRUE) {
 # load packages
packages <- c("raster", "geosphere", "gstat", "splancs", "dplyr", "ggplot2", "here", "knitr", "viridis")
lapply(packages, library, character.only=T)

  # Determine area of entire raster
  # Raster has 68523 cells
  y_bound <- distm(c(-121.3914, 38.2917), c(-121.3914, 38.31267))
  x_bound <- distm(c(-121.3914, 38.2917), c(-121.3686, 38.2917))
  # Calculate area in m^2
  area <- y_bound * x_bound
  # Total area is 4656185 m^2
  
  #deal with NA vals
  #complete.cases selects only the rows that are complete, eliminating all rows containing NA values
  #need to also do this in formatting function.. somehow
  begin <- begin[complete.cases(begin), ]
  end <- end[complete.cases(end), ]
  
  # define coordinate system
  coordinates(begin) <- ~ long + lat
  coordinates(end) <- ~ long + lat
  
  # load spatial domain to interpolate over
  grid <- raster(begin, ncols=100, nrows=100)
  
  # interpolate highs
  idw_begin <- gstat(formula = begin$level~1, locations = begin, nmax = 6, set = list(idp=2))
  idw_begin <- interpolate(grid, idw_begin)
  
  # interpolate lows
  idw_end <- gstat(formula = end$level~1, locations = end, nmax = 6, set=list(idp=2))
  idw_end <- interpolate(grid, idw_end)
  
  #plot begin and end to troubleshoot
  #plot(idw_end, main = "End")
  #plot(begin, add=T)
  #plot(idw_begin, main = "Start")
  
  # subtract to find net water levels throughout the event
  event_vol <- (idw_end-idw_begin)
  
  # import outline of boundary, as a set of points in lat/long that we generated in ArcMap as a digitized polyline
  bound <- read.csv(here::here("data", "bound_table.csv"), stringsAsFactors = FALSE, header = TRUE)
  bound_pts <- bound[,c(4,5)]
  colnames(bound_pts) <- c('x', 'y')
  
  # convert raster to SPDF, or at least get some coords
  event_pts <- coordinates(event_vol)[!is.na(values(event_vol)),]
  colnames(event_pts) <- c('x', 'y')
  
  # clip event idw by the innundation extent generated form our digitized polyline in ArcMap
  library(splancs)
  temp <- rasterToPoints(event_vol) %>% as.data.frame()
  event_clip <- temp[inout(event_pts, bound_pts), ]
  colnames(event_clip) <- c('x','y','z')
  
  
  coordinates(event_clip) <- ~ x + y
  gridded(event_clip) <- TRUE
  event_rast <- raster(event_clip, "z")
  

  library(rasterVis)
# plot with better scale 
# scale needs to be -1 to 8 when plotting events
# scale is 0 to 11 when plotting WY recharge
  par(mfcol=c(1,2)) #this is to get rid of padding around the raster
  plot(event_rast, 
       col = rev(bpy.colors(length(seq(0,11, by =1))-1)), 
       breaks = seq(0,11, by = 1),
       legend.args=
         list(text=expression(paste(Delta, ' Groundwater Elevation (m)')), 
              side=4, 
              font=20, 
              line=3, 
              cex=1),
       xlab = 'Longitude', ylab = 'Latitude')
 
  # calculate area per cell 
  m2_cell_idw <- 1739235/3746
  m2_cell_idw <- as.numeric(m2_cell_idw)
  
  # m3 per cell
  m3_idw <- m2_cell_idw * event_rast
  totm3_idw <- cellStats(m3_idw, "sum")
  
  # apply Sy range
  lowIDW <- totm3_idw*0.07
  midIDW <- totm3_idw*0.16
  highIDW <- totm3_idw*0.25
  return (midIDW)
  #result <-  matrix(c(lowIDW,midIDW,highIDW), ncol=1)
  #colnames(result) <- "Recharge Volume m^3"
  #rownames(result) <- c("low", "mid", "high")
  #result <- as.table(result)
  #return(result)
}
