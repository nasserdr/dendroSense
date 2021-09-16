library(readr)
library(lubridate)
library(dendRoAnalyst)
library(dplyr)
library(ggplot2)
library(dendrometeR) # for dendro specific data analysis

# install.packages("forecast")
# install.packages("curl")
# install.packages("dendrometeR")
idir = "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux"
setwd(idir)
dendro_df <- read.table("2603.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, skip = 12)
dendro_data <- dendro_df 


# time zone check 
# current time in Switzerland
Sys.time()
# current time in Belém (Brazil)
with_tz(Sys.time(), "Etc/GMT+3")


timeZone_data   = "Europe/Berlin" # incorrect time zone in which the data was taken
timeZone        = "Etc/GMT+3"     # correct time zone in Belém, Brazil


# Paste date and time together, replace month abreviations with english abreviations  
time_strings <- paste(dendro_data[, 1], dendro_data[, 2])
time_strings <- sub("M„r", "Mar", time_strings)
time_strings <- sub("Mai", "May", time_strings)
time_strings <- sub("Okt", "Oct", time_strings)
time_strings <- sub("Dez", "Dec", time_strings)

# convert data-time string to Posix time format 
# --> see ?strptime for how to specify the format argument 

dendro_data$Time_CET_CEST <- strptime(time_strings, format = "%d.%m.%Y %H:%M", tz = timeZone_data)

# convert to the correct timezone
dendro_data$Time_GMT <- with_tz(dendro_data$Time_CET_CEST, tz = timeZone_data)

# remove duplicated observations (duplicated time stamps in raw data)
dendro_data <- dendro_data[!duplicated(dendro_data$Time_GMT), ]

# Prepare 15 min time grid 

grid_start <- round_date(dendro_data$Time_GMT[1], "15 minutes")
grid_end   <- round_date(dendro_data$Time_GMT[nrow(dendro_data)], "15 minutes")

time_grid <- data.frame(gridTime = seq(grid_start, grid_end, 15 * 60))

# round to 15 min and remove duplicated timestamps (keep only first observation - these duplicates originate from rounding)

dendro_data$Time_GMT_round <- round_date(dendro_data$Time_GMT, "15 minutes")

message(paste("duplicates due to rounding:", length(duplicated(dendro_data$Time_GMT_round))))
message(paste("total number of time points:", length(dendro_data$Time_GMT_round)))

dendro_data <- dendro_data[!duplicated(dendro_data$Time_GMT_round), ]

# convert time columns to string with identical format (needed for the merging)

time_grid$gridTime <- format(time_grid$gridTime, "%Y-%m-%d %H:%M:%OS", tz = timeZone_data)

dendro_data$Time_GMT_round <- format(dendro_data$Time_GMT_round, "%Y-%m-%d %H:%M:%OS", tz = timeZone_data)

# Map to grid by merging 

dendro_data_on_grid <- merge(time_grid, dendro_data, by.x = "gridTime", by.y = "Time_GMT_round", all.x = TRUE)

# prepare data for dendrometeR 

my_dendro <- dendro_data_on_grid[, 4:7] # dendro data columns

rownames(my_dendro) <- dendro_data_on_grid$gridTime # timestamp as rownames

# inspect 

head(my_dendro) # top rows

tail(my_dendro) # bottom rows

is.dendro(my_dendro) # test whether my_dendro is in the format required by dendrometeR

my_dendro$datetime <- rownames(my_dendro)
rownames(my_dendro) <- NULL
my_dendro <- my_dendro[,c(5, 1,2,3, 4)]
names(my_dendro) <- c('datetime', 'T3B1', 'T3B3','T3B4', 'T3B2')
# my_dendro$date <- strptime(my_dendro$date, format = "%Y-%m-%d %H:%M:%OS")

my_dend1 <- spline.interpolation(my_dendro, resolution = 15, fill = TRUE)
daily_stats_dendro <- daily.data(my_dend1, TreeNum = 4)

#sc.phase<-phase.sc(df=gf_nepa17, TreeNum=1, smoothing=12, outputplot=TRUE, days=c(150,160))



day_s <- yday("2021-07-17")
day_e <- yday("2021-07-24")
sc_stats <- phase.sc(my_dend1, 
         TreeNum = 4, 
         smoothing = 1, 
         outputplot = TRUE, 
         days = c(day_s, day_e))

my_dend1$phase <- sc_stats$SC_phase$Phases
my_dend1$phase <- as.factor(my_dend1$phase)
my_dend1$phase <- recode(my_dend1$phase, "1" = "Shrinkage", "2" = "Expansion", "3" = "Increment")
na.omit(my_dend1)
my_dend_filterd <- my_dend1 %>% 
  filter(yday(datetime) <= day_e) %>% 
  filter(yday(datetime) >= day_s)
ggplot(my_dend_filterd, aes(datetime, T3B2)) +
  geom_point(aes(colour = factor(phase)), shape = 16, size= 3)+
  scale_color_manual(values = c("Shrinkage" = "red", "Expansion" = "#fcb854", "Increment" = "darkgreen")) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(color="#000000"),
    axis.text.y = element_text(color="#000000"),
    text=element_text(family = 'Arial', size = 14),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(size = 0.5),
    legend.key = element_rect(fill = "white"),
    legend.position = "top",
    strip.placement = 'inside',
    strip.background = element_blank()
  )  + 
  ylab(paste0("Stem diameter (","\U00B5", "m)")) + 
  xlab("Day") + 
  guides(
    colour=guide_legend(title="Phase"),
    )




zg_stats <- phase.zg(my_dend1, 
         TreeNum = 1,
         outputplot = TRUE,
         days = c(day_s, day_e))
cols <- c("1" = "red", "2" = "darkgreen", "3" = "orange")



plot(zg_stats$ZG_phase$TWD)

# Remove Median
daily_stats_dendro <- daily_stats_dendro %>% select(-median)

dendro_df$doy <- yday(my_dend1$date)
dendro_df$phase_sc <- sc_stats$SC_phase$Phases
#plot
ggplot(data = my_dend1, aes(x = date, y = T1A1)) + geom_line()

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

