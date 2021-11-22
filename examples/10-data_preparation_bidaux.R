
#_20211109_____________________________________________________________________________________________________________________________________________
library(dendRoAnalyst)
library(lubridate)
library(dplyr)
library(readr)

#Format for each agribase 15min
#Run all the scrip with each agribase_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________


idir <- "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/20210911_v4/raw_data"
dendro <- read.table(file.path(idir, "2773.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE, skip = 12, encoding = "latin1")

str(dendro)
head(dendro)
timeZone_data   = "Europe/Berlin" 
dendro <- na.omit(dendro)

head(dendro)
# Paste date and time together, replace month abreviations with english abreviations  
time_strings <- paste(dendro[, 1], dendro[, 2])
time_strings <- sub("M„r", "Mar", time_strings)
time_strings <- sub("Mai", "May", time_strings)
time_strings <- sub("Okt", "Oct", time_strings)
time_strings <- sub("Dez", "Dec", time_strings)

# convert data-time string to Posix time format 
# --> see ?strptime for how to specify the format argument 
dendro$Time_CET_CEST <- strptime(time_strings, format = "%d.%m.%Y %H:%M", tz = timeZone_data)

# convert to the correct timezone
#dendro_data$Time_GMT <- with_tz(dendro_data$Time_CET_CEST, tz = timeZone_data)

# remove duplicated observations (duplicated time stamps in raw data)
dendro <- dendro[!duplicated(dendro$Time_CET_CEST ), ]


# Prepare 15 min time grid 

grid_start <- round_date(dendro$Time_CET_CEST[1], "15 minutes")
grid_end   <- round_date(dendro$Time_CET_CEST[nrow(dendro)], "15 minutes")

time_grid <- data.frame(gridTime = seq(grid_start, grid_end, 15 * 60))

# round to 15 min and remove duplicated timestamps (keep only first observation - these duplicates originate from rounding)

dendro$Time_CET_CEST_round <- round_date(dendro$Time_CET_CEST, "15 minutes")

message(paste("duplicates due to rounding:", length(duplicated(dendro$Time_CET_CEST_round))))
message(paste("total number of time points:", length(dendro$Time_CET_CEST_round)))

dendro <- dendro[!duplicated(dendro$Time_CET_CEST_round), ]

# convert time columns to string with identical format (needed for the merging)

time_grid$gridTime <- format(time_grid$gridTime, "%Y-%m-%d %H:%M:%OS")

dendro$Time_CET_CEST_round <- format(dendro$Time_CET_CEST_round, "%Y-%m-%d %H:%M:%OS")

# Map to grid by merging 

dendro_data_on_grid <- merge(time_grid, dendro, by.x = "gridTime", by.y = "Time_CET_CEST_round", all.x = TRUE)

my_dendro <- dendro_data_on_grid
my_dendro$date <- NULL
#my_dendro <- na.omit(my_dendro)

colnames(my_dendro)[colnames(my_dendro) == "gridTime"] <- "date"
my_dendro$Time_CET_CEST <- NULL
my_dendro$heure <- NULL
head(my_dendro)
str(my_dendro)
my_dendro$date <- strptime(my_dendro$date, format="%Y-%m-%d %H:%M:%OS")
#my_dendro$date <- as.Date(my_dendro$date, "%Y-%m-%d %H:%M:%OS")

# Check the curve to see wich period will stay

plot(my_dendro$date, my_dendro$T1C4_20)

# Separate data by period

my_dendro_filter <- my_dendro %>% 
  filter(my_dendro$date > as.POSIXct("2021-06-20 01:00:00", tz="UTC"))

my_dendro_filter <- as.data.frame(my_dendro_filter)



head(my_dendro_filter)
str(my_dendro_filter)

idir = "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/20210911_v4/dendro_15min"
readr::write_csv(my_dendro_filter, file.path(idir, '2773_dendro_15min.csv'))

# dendRoAnalyst_________________________________________________________________________________________________________________________________________
# interpolation to run jumper function_______________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________
# _______________________________________________________________________________________________________________________________

df <- spline.interpolation(df= my_dendro_filter, resolution = 15, fill = TRUE)

idir = "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/20210911_v4/dendro_nogaps15min"
readr::write_csv(df, file.path(idir, '2773_nogaps_aligned15min.csv'))

