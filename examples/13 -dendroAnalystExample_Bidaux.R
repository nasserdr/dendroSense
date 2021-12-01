library(readr)
library(lubridate)
library(dendRoAnalyst)
library(dplyr)
library(ggplot2)
library(xts)
library(chron)
# install.packages("forecast")
# install.packages("curl")
# install.packages("dendrometeR")

# All files in one dataframe

#### Daily stats all dendros________________________________________________________________________________________________________________________
#### ________________________________________________________________________________________________________________________
#### ________________________________________________________________________________________________________________________
#### ________________________________________________________________________________________________________________________

idir = "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/2020/dendro_nogaps15min_nojumps"
odir = "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/2020"
files <- list.files(idir, pattern = '*.csv')

all_dendros <- NULL
for(file in files){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  #s <- substr(file, 1,4)
  s <- strsplit(file, '_') 

  s <- s[[1]][3]
  
  dendro_df[,1] <- as_datetime(dendro_df[,1], tz = "UTC", format = NULL)
  
  #dendro_df[,1] <- as.POSIXct(dendro_df[,1])
  #dendro_df[,1] <- as.POSIXct(dendro_df[,1], format= "%Y-%m-%d %H:%M:%OS", usetz= FALSE)
  
  #Daily_status
  daily_stats_dendro <- daily.data(dendro_df, TreeNum = 1)
  # Adding maximum daily shrinkage
  daily_stats_dendro <- daily_stats_dendro %>% 
    mutate(MDS = max - min, dendro = s)
  
  # Adding a temporary col to compute the DR
  daily_stats_dendro$temp <- c(daily_stats_dendro$max[2:nrow(daily_stats_dendro)], NA)
  
  # Computing the DR and selecting the needed cols
  # DR = MaxSD(Day + 1) - MinSD(Day)
  # Daily Growth (MXSD(Day + 1) - MXSD (Day))
  daily_stats_dendro <- daily_stats_dendro %>% 
    mutate(DR = temp - min, DG = temp - max) %>% 
    select(dendro, DOY, min, max, MDS, DG, DR)
  
  # Cumulative growth (simple cumulative function for the SGC)
  daily_stats_dendro <- daily_stats_dendro %>% 
    mutate(CumGrowth = cumsum(DR))
  
  all_dendros <- rbind(all_dendros, daily_stats_dendro)
}

# DOY in date
d_all <- all_dendros
d_all <- na.omit(d_all)
str(d_all)

all_dendros$date <-  as.Date(all_dendros$DOY, origin="2020-01-01")

write.table(all_dendros, file.path(odir, 'all_dendros_stats.csv'), row.names = FALSE, sep = ';')

#____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________

somePDFPath = "C:\\temp\\some.pdf"
pdf(file=somePDFPath)  

for (i in seq(5,10))   
{   
  par(mfrow = c(2,1))
  VAR1=rnorm(i)  
  VAR2=rnorm(i)  
  plot(VAR1,VAR2)   
} 
dev.off() 









#####Plot_____________________________________________________________________________________________________________________________
#####_____________________________________________________________________________________________________________________________
#####_____________________________________________________________________________________________________________________________
#####_____________________________________________________________________________________________________________________________
#####_____________________________________________________________________________________________________________________________

day_s <- yday("2021-07-06")
day_e <- yday("2021-08-06")
sc_stats <- phase.sc(all_dendros, 
                     TreeNum = 1, 
                     smoothing = 1, 
                     outputplot = TRUE, 
                     days = c(day_s, day_e))

all_dendros$phase <- sc_stats$SC_phase$Phases
all_dendros$phase <- as.factor(all_dendros$phase)
all_dendros$phase <- recode(all_dendros$phase, "1" = "Shrinkage", "2" = "Expansion", "3" = "Increment")
na.omit(all_dendros)
my_dend_filterd <- all_dendros %>% 
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




zg_stats <- phase.zg(all_dendros, 
                     TreeNum = 1,
                     outputplot = TRUE,
                     days = c(day_s, day_e),
                     ylab1 = "Stem size variation [µm]",
                     ylab2 = "TWD [µm]")
cols <- c("1" = "red", "2" = "darkgreen", "3" = "orange")



plot(zg_stats$ZG_phase$TWD)

# Remove Median
daily_stats_dendro <- all_dendros %>% select()

dendro_df$doy <- yday(all_dendros$date)
dendro_df$phase_sc <- sc_stats$SC_phase$Phases
#plot
ggplot(data = all_dendros, aes(x = date, y = T1A1)) + geom_line()

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

#' This code takes excel input files, each of which is a dendrometer signal (
#' one dendro per file only - the base and dendro name can be recognized from 
#' the file name) and does the following:
#' - Gather all the dataframe of all dendro together
#


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



