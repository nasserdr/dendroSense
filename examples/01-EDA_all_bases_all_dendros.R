#' This code takes excel input files, each of which is a dendrometer signal (
#' one dendro per file only - the base and dendro name can be recognized from 
#' the file name) and does the following:
#' - Gather all the dataframe of all dendro together
#

library(dendRoAnalyst)
library(lubridate)
library(dplyr)
library(xts)



idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-B-Jumps_fixed/"
files <- list.files(idir, pattern = '*.csv')

all_dendros <- NULL
for(file in files[2:length(files)]){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  s <- strsplit(file, '_') 
  base <- s[[1]][4]
  dendro <- s[[1]][6]
  
  daily_stats_dendro <- daily.data(dendro_df, TreeNum = 1)
  # Adding maximum daily shrinkage
  daily_stats_dendro <- daily_stats_dendro %>% 
    mutate(MDS = max - min, dendro = dendro, base = base)
  
  # Adding a temporary col to compute the DR
  daily_stats_dendro$temp <- c(daily_stats_dendro$max[2:nrow(daily_stats_dendro)], NA)
  
  # Computing the DR and selecting the needed cols
  # DR = MaxSD(Day + 1) - MinSD(Day)
  # Daily Growth (MXSD(Day + 1) - MXSD (Day))
  daily_stats_dendro <- daily_stats_dendro %>% 
    mutate(DR = temp - min, DG = temp - max) %>% 
    select(base, dendro, DOY, min, max, MDS, DG, DR)
  
  # Cumulative growth (simple cumulative function for the SGC)
  daily_stats_dendro <- daily_stats_dendro %>% 
    mutate(CumGrowth = cumsum(DR))

  all_dendros <- rbind(all_dendros, daily_stats_dendro)
}


odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Results"

write.table(all_dendros, file.path(odir, 'all_dendros_stats.csv'), row.names = FALSE, sep = ';')

# 
# idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/5-Jumps_fixed_partitioned"
# files <- list.files(idir, pattern = '*.csv')
# 
# file <- files[1]
# dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
# colnames(dendro_df) <- c('Time', 'T2')
# dendro_df$Date <- date(dendro_df$Time)
# 
# day_s = yday(dendro_df$Date[1])
# day_s
# day_e =yday(last(dendro_df$Date))
# day_e
# 
# zg_stats <- phase.zg(dendro_df,
#                       TreeNum = 1,
#                       outputplot = TRUE,
#                       days = c(day_s, day_e))

# # 
# dendro_xts <- as.xts(
#   x = dendro_df$T2,
#   order.by = dendro_df$Time
# )
# plot.xts(dendro_xts)
#     
#     
#     
#     
#   dates <- unique(date(dendro_df$Time))
#   dendro_df <- dendro_df[as.Date(dendro_df$Time) != last(dates),]
#   
#   # Problem with the sample numbers and missing data. There should be a gap filling thingy
#   daily_stats_dendro <- daily.data(dendro_day, TreeNum = 1)
#   sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12, outputplot=TRUE, days=c(150,160))
#   
#   dendro_day <- dendro_df[64:254,]
#   rownames(dendro_day) <- NULL
#   before <- dendro_day[1:65,]
#   bet <- data.frame(
#     Time = '2017-01-02 16:00:15',
#     T2 = 2000
#   )
#   after <- dendro_day[66:nrow(dendro_day),]
#   f <- rbind(before, bet, after)
#   sc_stats <- phase.sc(df = f, TreeNum = 1, smoothing = 12, outputplot=TRUE, days = c(date("2017-01-03"), date("2017-01-04")))
# 
#   dendro_df$date <- as.Date(dendro_df$Time)
#   s <- dendro_df %>% group_by(date) %>% tally()
#   zg_stats <- phase.zg(dendro_df, 
#                        TreeNum = 1,
#                        outputplot = TRUE,
#                        days = c(1,2))
#   
#   
#   plot(zg_stats$ZG_phase$TWD)
#   
#   # Remove Median
#   daily_stats_dendro <- daily_stats_dendro %>% select(-median)
#   
#   dendro_df$doy <- yday(dendro_df$date)
#   dendro_df$phase_sc <- sc_stats$SC_phase$Phases
#   #plot
#   ggplot(data = dendro_df, aes(x = date, y = T1A1)) + geom_line()
#   
#   # DOY = days
#   # Add Daily Net Daily Max shrinkage (DOY+1) - Daily Max shrinkage (DOY). First day being an NA
#   # Add Recovery: Daily Max shrinkage (DOY+1) - Daily Max shrinkage (DOY)
#   
#   # Create the "date" equivalent to "yday"
#   daily_stats_dendro$Date <- as.Date(daily_stats_dendro$DOY)
#   
#   # Tree Water Deficit (average and max) - To be taken from the ZG_phase
#   daily_stats_dendro$Max.twd <- zg_stats$ZG_cycle$Max.twd
#   daily_stats_dendro$Avg.twd <- zg_stats$ZG_cycle$Avg.twd
#   #
#   # # Add phases and duration_h (to be taken from ZG_Phase)
#   daily_stats_dendro$phases <- zg_stats$ZG_cycle$Phases
#   daily_stats_dendro$duration_h <- zg_stats$ZG_cycle$Duration_h
#   
# }
# 
# 
# file <- files[1]
# stat_dendro(file)
# 
# 
# library(dendRoAnalyst)
# data(gf_nepa17)
# sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12, outputplot=TRUE, days=c(150,160))


