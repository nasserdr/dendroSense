#' This code reads the filtered signals (from Clean_signals_gathered), remove 
#' the jumps and then save the signals without jumps in Jumps_fixed
#' This code takes into account that the data come with a timezone of Brazil and 
#' therefore it correct the timezone of the data

library(readr)
library(lubridate)
library(dendRoAnalyst)
library(dplyr)
library(ggplot2)

# -----------------------------------------------
# time zone check 
# current time in Switzerland
Sys.time()
# current time in Belém (Brazil)
with_tz(Sys.time(), "Etc/GMT+3")
# -----------------------------------------------


#Settings and discovery of the list of files
idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/2-Clean_signals_gathered"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-B-Jumps_fixed"
files <- list.files(idir, pattern = '*.csv')
timeZone_data   = "Europe/Berlin" # incorrect time zone in which the data was taken
timeZone        = "Etc/GMT+3"     # correct time zone in Belém, Brazil

####################################
######### FUNCTION 1 - Loading data#
####################################
#Reading the data and sampling them to 15 min
load_data <- function(index){
  dendro_file <- files[index] # Change from 1 to 9 (we have 9 files)
  # the 3 did not work
  dendro <- read.table(file.path(idir, dendro_file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  
  dendro_tz_new <- dendro %>% 
    mutate(Time_CET_CEST = strptime(dendro$datetime, format = "%Y-%m-%d %H:%M:%OS", tz = timeZone_data))
  
  dendro_tz_new <- dendro_tz_new %>% 
    mutate(Time_GMT = with_tz(Time_CET_CEST, tzone = timeZone))
  
  
  # dendro$Time_CET_CEST <- strptime(dendro$datetime, format = "%Y-%m-%d %H:%M:%OS", tz = timeZone_data)
  
  # convert to the correct timezone
  # dendro$Time_GMT <- with_tz(dendro$Time_CET_CEST, tz = timeZone)
  
  # dendro$Time_GMT <- strptime(dendro$Time_GMT, format = "%Y-%m-%d %H:%M:%OS")
  grid_start <- round_date(dendro_tz_new$Time_GMT[1], "15 minutes")
  grid_end   <- round_date(dendro_tz_new$Time_GMT[nrow(dendro_tz_new)], "15 minutes")
  
  time_grid <- data.frame(gridTime = seq(grid_start, grid_end, 15 * 60))
  
  # round to 15 min and remove duplicated timestamps (keep only first observation - these duplicates originate from rounding)
  
  dendro_tz_new$Time_GMT_round <- round_date(dendro_tz_new$Time_GMT, "15 minutes")
  
  message(paste("duplicates due to rounding:", length(duplicated(dendro$Time_GMT_round))))
  message(paste("total number of time points:", length(dendro$Time_GMT_round)))
  
  dendro_tz_new <- dendro_tz_new[!duplicated(dendro_tz_new$Time_GMT_round), ]
  
  # convert time columns to string with identical format (needed for the merging)
  
  time_grid$gridTime <- format(time_grid$gridTime, "%Y-%m-%d %H:%M:%OS")
  
  dendro_tz_new$Time_GMT_round <- format(dendro_tz_new$Time_GMT_round, "%Y-%m-%d %H:%M:%OS")
  
  # Map to grid by merging 
  
  dendro_data_on_grid <- merge(time_grid, dendro_tz_new, by.x = "gridTime", by.y = "Time_GMT_round", all.x = TRUE)
  
  my_dendro <- dendro_data_on_grid
  my_dendro$date <- NULL
  my_dendro <- na.omit(my_dendro)
  
  colnames(my_dendro)[colnames(my_dendro) == "gridTime"] <- "datetime"
  my_dendro <- my_dendro[, c(1,3)]
  return(my_dendro)
}

####################################
######### FUNCTION 2 - Saving data#
####################################

save_data <- function(jump_free_dendro, base, dendro, index){
  jump_free_dendro$datetime <- as.POSIXct(jump_free_dendro$datetime, format = "%Y-%m-%d %H:%M:%OS")
  dendro_file <- files[index]
  
  names <- unlist(strsplit(dendro_file, "_"))
  base <- names[2]
  dendro <- names[4]
  
  
  write.csv(x = jump_free_dendro,
            file = file.path(odir, paste0('Jumps_fixed_', dendro_file)),
            row.names = FALSE)
  
  p <- ggplot(data = jump_free_dendro, 
              aes(x = datetime, y = value)) +
    xlab('Date') + 
    ylab ("Dendro Value") + 
    geom_line() + 
    ggtitle(paste('Ag', base, 'dendro', dendro)) + 
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(color="#000000"),
      axis.text.y = element_text(color="#000000"),
      text=element_text(family = 'Arial', size = 8),
      panel.background = element_rect(fill = "white"),
      axis.line = element_line(size = 0.5),
      legend.key = element_rect(fill = "white"),
      strip.placement = 'inside',
      strip.background = element_blank()
    )
  
  ggsave(
    filename = file.path(odir, paste0('Jumps_fixed_Base_', base, '_Dendro_', dendro, '_Filtered.tiff')),
    p,
    device = 'tiff',
    width = 16,
    height = 7,
    units = "cm",
    dpi = 400
  )
  dev.off()
}


################################
######### RUN WITH INDEX from 1#
################################

index <- 1 #Change from 1 to 29
my_dendro <- load_data(index)
my_dendro$value <- my_dendro$value/1000
jump_free_dendro <- jump.locator(my_dendro[2:nrow(my_dendro),], TreeNum = 1, v = 0.3)


save_data(jump_free_dendro, base, dendro, index)




