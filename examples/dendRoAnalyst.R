library(dendRoAnalyst)
library(lubridate)
library(dplyr)


setwd("~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/dendRoAnalyst")
dendro <- read.table("1811.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

dendro$date <- strptime(dendro$date, format = "%Y-%m-%d %H:%M:%OS")
str(df)



# Prepare 15 min time grid 

grid_start <- round_date(dendro$date[1], "15 minutes")
grid_end   <- round_date(dendro$date[nrow(dendro)], "15 minutes")

time_grid <- data.frame(gridTime = seq(grid_start, grid_end, 15 * 60))

# round to 15 min and remove duplicated timestamps (keep only first observation - these duplicates originate from rounding)

dendro$Time_GMT_round <- round_date(dendro$date, "15 minutes")

message(paste("duplicates due to rounding:", length(duplicated(dendro$Time_GMT_round))))
message(paste("total number of time points:", length(dendro$Time_GMT_round)))

dendro <- dendro[!duplicated(dendro$Time_GMT_round), ]

# convert time columns to string with identical format (needed for the merging)

time_grid$gridTime <- format(time_grid$gridTime, "%Y-%m-%d %H:%M:%OS")

dendro$Time_GMT_round <- format(dendro$Time_GMT_round, "%Y-%m-%d %H:%M:%OS")

# Map to grid by merging 

dendro_data_on_grid <- merge(time_grid, dendro, by.x = "gridTime", by.y = "Time_GMT_round", all.x = TRUE)

my_dendro <- dendro_data_on_grid
my_dendro$date <- NULL
my_dendro <- na.omit(my_dendro)

colnames(my_dendro)[colnames(my_dendro) == "gridTime"] <- "date"

# dendRoAnalyst_________________________________________________________________________________________________________________________________________

gf_dendro <- spline.interpolation(df=my_dendro, resolution = 15, fill = FALSE)
df1 <- gf_dendro 

# Creating an example reference dataset 

df2 < - cbind(gf_dendro,gf_dendro[,2:3],gf_dendro[,4:5]) 


# Using proportional interpolation method

df1_NI <- 
gf_nepa17 



jump_free_dendro <- jump.locator(my_dendro, TreeNum = 1, v= 800)
jump_free_dendro <- jump.locator(my_dendro, TreeNum = 1, v= -800)
head(jump_free_dendro)

gf_dendro <- spline.interpolation(df=my_dendro, resolution = 15, fill = TRUE)
df1_NI <- network.interpolation(my_dendro)
daily_dendro <- daily.data(gf_dendro, TreeNum = 1)
sc.phase <- phase.sc(gf_dendro, TreeNum = 1, smoothing = 4, outputplot= TRUE, days = c(166,173))
zg.phase <- phase.zg(gf_dendro, TreeNum = 1, outputplot= TRUE, days = c(166,173))




