library(readr)
library(lubridate)
library(dendRoAnalyst)
source("startup.R")

dendro_df <- read.table("data/2605_2021.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

dendro_df$date <- strptime(dendro_df$date, format = "%Y-%m-%d %H:%M:%OS")

daily_stats_dendro <- daily.data(dendro_df, TreeNum = 1)

day_s = yday("2021-06-15")
day_e = yday("2021-06-22")
sc_stats <- phase.sc(dendro_df, 
         TreeNum = 1, 
         smoothing = 1, 
         outputplot = TRUE, 
         days = c(day_s, day_e))


zg_stats <- phase.zg(dendro_df, 
         TreeNum = 1,
         outputplot = TRUE,
         days = c(day_s, day_e))

# Remove Median
daily_stats_dendro <- daily_stats_dendro %>% select(-median)

dendro_df$doy <- yday(dendro_df$date)
dendro_df$phase_sc <- sc_stats$SC_phase$Phases
#plot
ggplot(data = dendro_df, aes(x = date, y = T1A1)) + geom_line()

# Add Daily Net Growth Max(previous day) - Max(current day). First day being an NA
# Add Recovery: Daily Max (PM) - Daily Min (PM)

# Create the "date" equivalent to "yday"
daily_stats_dendro$Date <- as.Date(daily_stats_dendro$DOY)

# Tree Water Deficit (average and max) - To be taken from the ZG_phase
# daily_stats_dendro$Max.twd <- zg_stats$ZG_cycle$Max.twd
# daily_stats_dendro$Avg.twd <- zg_stats$ZG_cycle$Avg.twd
# 
# # Add phases and duration_h (to be taken from ZG_Phase)
# daily_stats_dendro$phases <- zg_stats$ZG_cycle$Phases
# daily_stats_dendro$duration_h <- zg_stats$ZG_cycle$Duration_h

