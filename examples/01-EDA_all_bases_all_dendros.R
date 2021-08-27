library(dendRoAnalyst)
library(lubridate)
library(dplyr)


idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Jumps_fixed"
files <- list.files(idir, pattern = '*.csv')
file <- files[1]


# stat_dendro <- function(file){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  # Problem with the dates (it's not a list)
  dates <- as.Date(dendro_df$Time, format = "%Y-%m-%d %H:%M:%S")
  day1 <- as.character(dates[2])
  day2 <- as.character(last(dates))
  dendro_df <- dendro_df[as.Date(dendro_df$Time) == dates[4],]
  dendro_df <- dendro_df[as.Date(dendro_df$Time) != last(dates),]
  
  # Problem with the sample numbers and missing data. There should be a gap filling thingy
  daily_stats_dendro <- daily.data(dendro_df, TreeNum = 1)
  sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12, outputplot=TRUE, days=c(150,160))
  
  sc_stats <- phase.sc(df = dendro_df, TreeNum = 1, smoothing = 12, outputplot=TRUE, days=c(150,160))

  dendro_df$date <- as.Date(dendro_df$Time)
  s <- dendro_df %>% group_by(date) %>% tally()
  zg_stats <- phase.zg(dendro_df, 
                       TreeNum = 1,
                       outputplot = TRUE,
                       days = c(1,2))
  
  
  plot(zg_stats$ZG_phase$TWD)
  
  # Remove Median
  daily_stats_dendro <- daily_stats_dendro %>% select(-median)
  
  dendro_df$doy <- yday(dendro_df$date)
  dendro_df$phase_sc <- sc_stats$SC_phase$Phases
  #plot
  ggplot(data = dendro_df, aes(x = date, y = T1A1)) + geom_line()
  
  # DOY = days
  # Add Daily Net Daily Max shrinkage (DOY+1) - Daily Max shrinkage (DOY). First day being an NA
  # Add Recovery: Daily Max shrinkage (DOY+1) - Daily Max shrinkage (DOY)
  
  # Create the "date" equivalent to "yday"
  daily_stats_dendro$Date <- as.Date(daily_stats_dendro$DOY)
  
  # Tree Water Deficit (average and max) - To be taken from the ZG_phase
  daily_stats_dendro$Max.twd <- zg_stats$ZG_cycle$Max.twd
  daily_stats_dendro$Avg.twd <- zg_stats$ZG_cycle$Avg.twd
  #
  # # Add phases and duration_h (to be taken from ZG_Phase)
  daily_stats_dendro$phases <- zg_stats$ZG_cycle$Phases
  daily_stats_dendro$duration_h <- zg_stats$ZG_cycle$Duration_h
  
}


file <- files[1]
stat_dendro(file)


library(dendRoAnalyst)
data(gf_nepa17)
sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12, outputplot=TRUE, days=c(150,160))
