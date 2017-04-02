#_______________________________________________________________________________
## Libraries
library(tidyverse)
library(scales)
library(viridis)
library(plotly)
#_______________________________________________________________________________

#_______________________________________________________________________________
# read one upas file
# file <- "data/log/PS108LOG_170116_123618_000000_000000_BLR02___________HHH.txt"
load_upas_file <- function(file, filename){
 software_info <- read_csv(file, col_names = "versions",
                           n_max = 1, col_types = NULL)
 sample_info <- read_csv(file, col_names = TRUE,
                         n_max = 1, skip = 1, col_types = NULL)
 col_names <- read_csv(file, col_names = FALSE,
                       n_max = 1, skip = 4, col_types = NULL)
 units <- read_csv(file, col_names = as.character(col_names[1,]),
                   n_max = 1, skip = 3, col_types = NULL)
 data <- read_csv(file, col_names = TRUE, skip = 4)
 # parse file name
 data <- dplyr::mutate(data, sample = strsplit(basename(filename), "_")[[1]][6])
 
 # parse header info to main data file
 data <- dplyr::mutate(data, start = as.POSIXct(as.character(sample_info$StartTime[1]),
                                                format = "%y%m%d%H%M%S"),
                       end = as.POSIXct(as.character(sample_info$EndTime[1]),
                                        format = "%y%m%d%H%M%S"))
 # convert data classes
 data <- dplyr::mutate(data, datetime = as.POSIXct(as.character(timestr),
                                                   format = "%y%m%d%H%M%S"))
 # rename columns
 data <- dplyr::rename(data, flow = volflow,
                       vol = sampledVol,
                       t_oc = bme_temp,
                       p_kpa = bme_press,
                       rh_pct = bme_rh,
                       den = atmoRho,
                       dp = dpSDPu25,
                       t_oc_sd = tempSDPu25,
                       bat_v = bVolt,
                       bat_fuel = bFuel,
                       lat = gpslatitude,
                       lng = gpslongitude,
                       gps_date = gpsUTCDate,
                       gps_time = gpsUTCTime,
                       gps_sat = gpssatellites,
                       gps_alt = gpsaltitude,
                       id = sample)
 # return
 return(data)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Load multifile folders
# out <- load_multifile("data/log", "*")
# saveRDS(out, "upas_india.rds")
load_multifile <- function(files, names){

 # loop files
 for(i in 1:length(files[,1])){
  ifelse(i==1,
         out <- load_upas_file(files[[i]], names[[i]]),
         out <- rbind(out, load_upas_file(files[[i]], names[[i]])))
 }
 # return
 return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Clean gps
upas_gps <- function(df){
 out <- dplyr::mutate(df, lat = replace(lat, gps_sat < 3, NA)) %>%
  dplyr::mutate(lng = replace(lng, gps_sat < 3, NA)) %>%
  dplyr::mutate(gps_alt = replace(gps_alt, gps_sat < 3, NA)) %>%
  dplyr::mutate(gps_date = replace(gps_date, gps_sat < 3, NA)) %>%
  dplyr::mutate(gps_time = replace(gps_time, gps_sat < 3, NA))
 # return
 return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Process upas
# upas <- upas_process(upas)
upas_process <- function(df){
 out <- upas_gps(df)
 # return
 return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot PM
# plot_pm(upas, "BLR02")
plot_pm <- function(df){

 # plot
 p <- ggplot(df, aes(x = datetime, y = dp, color = id)) +
      geom_point() +
      theme_minimal() +
      xlab("") +
      ylab("Pressure drop (Pa)") +
      ylim(0, round_up(max(df$dp, na.rm = TRUE))) +
      theme(text = element_text(size = 22)) +
      scale_fill_brewer() +
      theme(legend.position = "none")
 # return
 return(p)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot met
# plot_met(upas, "BLR02")
plot_met <- function(df){
 # plot t
 p_t <- ggplot(df, aes(x = datetime, y = t_oc, color = id)) +
  geom_point() +
  theme_minimal() +
  xlab("") +
  ylab("Temp (oC)") +
  ylim(0, round_up(max(df$t_oc, na.rm = TRUE))) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 22)) +
  scale_fill_brewer() +
  theme(legend.title=element_blank())
 # plot rh
 p_rh <- ggplot(df, aes(x = datetime, y = rh_pct, color = id)) +
  geom_point() +
  theme_minimal() +
  xlab("") +
  ylab("RH (%)") +
  ylim(0, round_up(max(df$rh_pct, na.rm = TRUE))) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 22)) +
  scale_fill_brewer() +
  theme(legend.title=element_blank())

 # plot p
 p_p <- ggplot(df, aes(x = datetime, y = p_kpa, color = id)) +
  geom_point() +
  theme_minimal() +
  xlab("") +
  ylab("Atmos-P (kPa)") +
  ylim(round(min(df$p_kpa, na.rm = TRUE) -50, -2),
       round(max(df$p_kpa, na.rm = TRUE) + 50, -2)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 22)) +
  scale_fill_brewer() +
  theme(legend.title=element_blank())
 # combine
 p <- multiplot(p_t, p_rh, p_p, cols = 1)
 
 # return
 return(p)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot operation flow
# plot_op_flow(upas, "BLR02")
plot_op_flow <- function(df){
 # plot flow
  p_flow <- ggplot(df, aes(x = datetime, y = flow, color = id)) +
            geom_point() +
            theme_minimal() +
            xlab("") +
            ylab("Flow (L/min)") +
            ylim(floor(min(df$flow)),
                 ceiling(max(df$flow))) +
            theme(legend.position = "none") +
            theme(text = element_text(size = 20)) +
            scale_fill_brewer() +
            theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
 # return
  return(p_flow)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot operation volume
# plot_op_flow(upas, "BLR02")
plot_op_vol <- function(df){ 
 # plot sampled volume
  p_vol <- ggplot(df, aes(x = datetime, y = vol, color = id)) +
           geom_point() +
           theme_minimal() +
           xlab("") +
           ylab("Sample (L)") +
           ylim(0, round_up(max(df$vol, na.rm = TRUE))) +
           theme(legend.position = "none") +
           theme(text = element_text(size = 20)) +
           scale_fill_brewer() +
           theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
 # return
  return(p_vol)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot operation battery voltage
plot_op_batv <- function(df){ 
 # plot battery voltage
  p_batv <- ggplot(df, aes(x = datetime, y = bat_v, color = id)) +
            geom_point() +
            theme_minimal() +
            xlab("") +
            ylab("Bat (V)") +
            ylim(round(min(df$bat_v, na.rm = TRUE) -50, -2),
                 round(max(df$bat_v, na.rm = TRUE) + 50, -2)) +
            theme(legend.position = "none") +
            theme(text = element_text(size = 20)) +
            scale_fill_brewer() +
            theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
 # return
  return(p_batv)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# Plot operation battery fuel
plot_op_batf <- function(df){ 
 # plot battery fuel
  p_batf <- ggplot(df, aes(x = datetime, y = bat_fuel, color = id)) +
            geom_point() +
            theme_minimal() +
            xlab("") +
            ylab("Fuel (?)") +
            ylim(round(min(df$bat_fuel, na.rm = TRUE) -50, -2),
                 round(max(df$bat_fuel, na.rm = TRUE) + 50, -2)) +
            theme(legend.position = "none") +
            theme(text = element_text(size = 20)) +
            scale_fill_brewer() +
            theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
 # return
  return(p_batf)
}
#_______________________________________________________________________________
 # combine
 # p <- multiplot(p_flow, p_vol, p_batv, p_batf, cols = 1)
 # return
 # return(p)
 #}
#_______________________________________________________________________________

#_______________________________________________________________________________
# round down order of magnitude
round_down <- function(x) suppressWarnings(ifelse(is.na(10^floor(log10(x))),
                                                  x,
                                                  10^floor(log10(x))))
# round down order of magnitude
round_up <- function(x) suppressWarnings(ifelse(is.na(10^ceiling(log10(x))),
                                                x,
                                                10^ceiling(log10(x))))
#_______________________________________________________________________________

#_______________________________________________________________________________
# Multiple plot function from R cookbook
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
 library(grid)
 
 # Make a list from the ... arguments and plotlist
 plots <- c(list(...), plotlist)
 
 numPlots = length(plots)
 
 # If layout is NULL, then use 'cols' to determine layout
 if (is.null(layout)) {
  # Make the panel
  # ncol: Number of columns of plots
  # nrow: Number of rows needed, calculated from # of cols
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols))
 }
 
 if (numPlots==1) {
  print(plots[[1]])
  
 } else {
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
   # Get the i,j matrix positions of the regions that contain this subplot
   matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
   
   print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                   layout.pos.col = matchidx$col))
  }
 }
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# map gps data
upas_map <- function(df, col, id_list = ""){
 print(col)
 # clean
 df <- dplyr::filter(df, !is.na(lat))

 # plot
 map <- leaflet(df) %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE))
 # color by id
 if(col == "id"){
  # color pallete
    color <- colorFactor(rainbow(length(unique(df$id))), df$id)
  # update map object
    map <- addCircleMarkers(map, radius = 5,
                            color=~color(id),
                            stroke = FALSE, fillOpacity = 0.5) %>%
           addLegend(pal = color, values = ~id,
                     opacity = 1, title = "UPAS ID")
 }
 # color by t
 if(col == "t"){
  # color pallete
  pal <- colorNumeric(palette = c("blue", "red"), domain = df$t_oc)
  # update map object
  map <- addCircleMarkers(map, radius = 5,
                          color= ~pal(t_oc),
                          stroke = FALSE, fillOpacity = 0.5) %>%
   addLegend(pal = pal,
             values = ~t_oc,
             opacity = 1,
             title = "T (oC)")
 }
# color by rh
 if(col == "rh"){
  # color pallete
  pal <- colorNumeric(palette = c("red", "blue"), domain = df$rh_pct)
  # update map object
  map <- addCircleMarkers(map, radius = 5,
                          color= ~pal(rh_pct),
                          stroke = FALSE, fillOpacity = 0.5) %>%
   addLegend(pal = pal,
             values = ~rh_pct,
             opacity = 1,
             title = "RH (%)")
 }
 # color by dp
 if(col == "dp"){
  # color pallete
  pal <- colorNumeric(palette = "YlGnBu", domain = df$dp)
  # update map object
  map <- addCircleMarkers(map, radius = 5,
                          color= ~pal(dp),
                          stroke = FALSE, fillOpacity = 0.5) %>%
   addLegend(pal = pal,
             values = ~dp,
             opacity = 1,
             title = "Dp (kPa)")
 }
 # return
  return(map)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# select met data
data_met <- function(df){
  out <- # dplyr::filter(df, id == id_inst) %>%
           dplyr::select(df, t_oc, rh_pct, p_kpa, id, datetime)
  # return
    return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# select op data
data_op <- function(df){
  out <- # dplyr::filter(df, id == id_inst) %>%
           dplyr::select(df, flow, vol, bat_v, bat_fuel, id, datetime)
  # return
 return(out)
}
#_______________________________________________________________________________

#_______________________________________________________________________________
# select pm data
data_pm <- function(df){
 out <- # dplyr::filter(df, id == id_inst) %>%
  dplyr::select(df, dp, id, datetime)
 # return
 return(out)
}
#_______________________________________________________________________________

plot_hist_dp <- function(df){
 # plot
 p <- ggplot(df, aes(x = dp, fill = id)) +
  geom_histogram(binwidth = 0.5) +
  theme_minimal() +
  xlab("dp (kPa)") +
  ylab("frequency") +
  theme(text = element_text(size = 22), legend.position="none") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
 # return
 return(p)
}
#_______________________________________________________________________________