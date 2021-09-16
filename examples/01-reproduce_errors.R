#' Sep 09, 2021, roland.nasser@agroscope.admin.ch 
#' This code was done in order to create a reproducible error for seeking help
#' from the library creator. 
#' Message sent at 18h00 CET
#' 
#' In this code, we load dendro data from 3 files and compute the ZG cycle in 
#' order to show the error produced by each file.
#' All files are placed in the same directory as the code.


#Example with a good file

good_one <- read.csv("dendro_good.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
colnames(good_one) <- c('Time', 'T2')
good_one$Time <- as.POSIXct(good_one$Time, format = "%Y-%m-%d %H:%M:%OS")
good_one$Date <- date(good_one$Time)

day_s = yday(good_one$Date[1])
day_s
day_e =yday(last(good_one$Date))
day_e

zg_stats_good <- phase.zg(good_one,
                     TreeNum = 1,
                     outputplot = FALSE,
                     days = c(day_s, day_e))

#Example with a file producing the following error:
# 
dendro_na_error <- read.csv("dendro_gives_na_error.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
colnames(dendro_na_error) <- c('Time', 'T2')
dendro_na_error$Time <- as.POSIXct(dendro_na_error$Time, format = "%Y-%m-%d %H:%M:%OS")
dendro_na_error$Date <- date(dendro_na_error$Time)

day_s = yday(dendro_na_error$Date[1])
day_s
day_e =yday(last(dendro_na_error$Date))
day_e

zg_stats_na <- phase.zg(dendro_na_error,
                          TreeNum = 1,
                          outputplot = FALSE,
                          days = c(day_s, day_e))

#Example with a file producing the following error:
# 
dendro_shorter_error <- read.csv("dendro_gives_shorter_error.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
colnames(dendro_shorter_error) <- c('Time', 'T2')
dendro_shorter_error$Time <- as.POSIXct(dendro_shorter_error$Time, format = "%Y-%m-%d %H:%M:%OS")
dendro_shorter_error$Date <- date(dendro_shorter_error$Time)

day_s = yday(dendro_shorter_error$Date[1])
day_s
day_e =yday(last(dendro_shorter_error$Date))
day_e

zg_stats_shorter <- phase.zg(dendro_shorter_error,
                          TreeNum = 1,
                          outputplot = FALSE,
                          days = c(day_s, day_e))
