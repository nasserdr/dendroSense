#' This code transform data files (with missing days) to single files with no
#' missing days nor missing data. There are 3 parts of code in this file:
#' A- Check the number of sample per day and explore it via a histogram.
#' B- Remove the days where the number of data points is less than 80 (96 points
#' being the maximum because we have 4 samples per hour. 96 = 4 * 24). These files
#' are saved in the folder 'Jumps_Fixed_Partial_days_Removed'
#' C- Read the complete files, check for gaps in dates and divide the file into
#' several files for the same dendrometer 'Jumps_fixed_partitioned'
#' D- Fill the gaps using the dendrometers data where the new file contains a 
#' grid of time of full 96 samples per day. Files saved in 'Jumps_fixed_partitioned_no_gaps'
#' E - Compute the parameters from the 3 approaches on the all the files


library(dendRoAnalyst)
library(lubridate)
library(dplyr)
library(tidyverse)
library(xts)

#############################################################################################
##########PART A: create a histogram for the number of data points per day for all the files#
#############################################################################################

idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-B-Jumps_fixed"
files <- list.files(idir, pattern = '*.csv')

counts <- NULL
for(file in files){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  dendro_df$Date <- date(dendro_df$Time)
  s <- dendro_df %>% group_by(Date) %>% tally() 
  counts <- c(counts, s$n)
}

hist(counts)
#From the counts histogram, we see that we need to remove days when n <80

#############################################################################################
##########PART A1: Gather all dendros of each base in one dataframe ##########################
#############################################################################################
idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-B-Jumps_fixed"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-A-Data_per_base"

files <- list.files(idir, pattern = '*.csv')

fs <- strsplit(files, "_")
fs_df <- data.table::transpose(as.data.frame(fs))

couples <- fs_df %>% dplyr::select(4,6)
couples <- distinct(couples)
names(couples) <- c('base', 'dendro')
base <- couples$base[1]
base

for (base in unique(couples$base)){
  list_base <- files[grepl(files, pattern = base)]
  file <- list_base[1]
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  s <- strsplit(file, '_')
  dendro <- s[[1]][6]
  
  colnames(dendro_df) <- c('Time', paste0("T", dendro))
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  all_dendros <- dendro_df
  file <- files[2]
  for(file in files[2:length(list_base)]){
    dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
    s <- strsplit(file, '_')
    dendro <- s[[1]][6]
    colnames(dendro_df) <- c('Time', paste0("T", dendro))
    dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
    all_dendros <- dplyr::full_join(all_dendros, dendro_df, by = "Time")
  }
  
  write.csv(x = as.data.frame(all_dendros),
            file = file.path(odir, paste0("Base_", base, ".csv")),
            row.names = FALSE)
}

#############################################################################################
##########PART B: Remove days with less than 80 data points per day #########################
#############################################################################################
idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-B-Jumps_fixed"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/4-Jumps_Fixed_Partial_days_Removed"

files <- list.files(idir, pattern = '*.csv')
file <- files[1]
for(file in files){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  dendro_df$Date <- date(dendro_df$Time)
  samples_per_day <- dendro_df %>% group_by(Date) %>% tally()
  date_to_keep <- samples_per_day$Date[samples_per_day$n >= 80]
  dendro_df_days_to_keep <- dendro_df %>% 
    filter(Date %in% date_to_keep) %>% 
    dplyr::select(Time, T2)
  write.csv(x = as.data.frame(dendro_df_days_to_keep),
            file = file.path(odir, file), sep = ",",
            row.names = FALSE)
}

#############################################################################################
##########PART C: Read the complete files and partition them when there is a gap 
# of more than 24 hours
#############################################################################################
idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/4-Jumps_Fixed_Partial_days_Removed"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/5-Jumps_fixed_partitioned"
files <- list.files(idir, pattern = '*.csv')
file <- files[1]
split_file_with_empty_days <- function(file, idir, odir){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  days <- unique(date(dendro_df$Time))
  d1 <- which(diff(days) != 1) # Detect days where dates are not consecutive
  d2 <- d1 + 1 
  ranges <- data.frame(
    start = c(days[1], days[d2]), # Start of days splits
    end = c(days[d1], last(days)) # End of the days splits
  )
  split <-1
  for(i in 1:nrow(ranges)){
      dendro_part <- dendro_df %>% 
        filter( date(Time) >= ranges$start[i]) %>% 
        filter( date(Time) <= ranges$end[i])
      write.table(dendro_part, file.path(odir, paste0(substr(file, 1,nchar(file)-4), 'split_', i,'.csv')), row.names = FALSE, sep = ';')
  }
}

# split_file_with_empty_days(files[1], idir, odir)

for(file in files){
  split_file_with_empty_days(file, idir, odir)
}

#Verify that all splits are correct
for(file in list.files(odir)){
  dendro_df <- read.csv(file.path(odir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  days <- unique(date(dendro_df$Time))
  if(length(which(diff(days) != 1)) != 0){
    message(paste0('File ', file, ' is NOT correctly splitted'))
  }else{
    print(paste0('File ', file, ' is correctly splitted'))
  }
}

#############################################################################################
##########PART D: Fill the gaps using the dendrometers data where the new file contains a ###
#' grid of time of full 96 samples per day. Files saved in 'Jumps_fixed_partitioned_no_gaps'#
#############################################################################################
idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/5-Jumps_fixed_partitioned"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/6-Jumps_fixed_partitioned_no_gaps"

files <- list.files(idir, pattern = ".csv")
file <- files[1]

for(file in files){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')

  dendro_df <- dendro_df  %>% distinct(Time, .keep_all = TRUE)
  dendro_df <- spline.interpolation(dendro_df, resolution = 15, fill = TRUE)
  dendro_df <- dendro_df[c(1,3)]
  colnames(dendro_df) <- c('Time', 'Value')
  
  
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS", tz= 'CET')
  write.table(dendro_df, file.path(odir, paste0(substr(file, 1,nchar(file)-4), '_no_gaps.csv')), row.names = FALSE, sep = ';')
  
}

#############################################################################################
##########PART E: Compute all the stats and save them in the folder Results #################
#############################################################################################

idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/6-Jumps_fixed_partitioned_no_gaps"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Results"

files <- list.files(idir, pattern = ".csv")
all_stats <- NULL
file <- files[1]
i <- 0
for(file in files){
  i <- i + 1
  print(paste(
    "Processing file",
    file,
    "Code",
    i
  ))
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  dendro_df$Date <- date(dendro_df$Time)
  s <- strsplit(file, '_')
  base <- s[[1]][4]
  dendro <- s[[1]][6]
  
  daily_stats_dendro <- daily.data(dendro_df, TreeNum = 1)
  
  # Computing the DR and selecting the needed cols
  # DR = MaxSD(Day + 1) - MinSD(Day)
  # Daily Growth (MXSD(Day + 1) - MXSD (Day))
  # Cumulative growth (simple cumulative function for the SGC)
  # Also, adding date
  if(nrow(daily_stats_dendro) > 1){ # Some variables can^t be computed if we have only one day of daily stats
    # Adding maximum daily shrinkage
    daily_stats_dendro <- daily_stats_dendro %>%
      mutate(MDS = max - min, dendro = dendro, base = base)
    
    # Adding a temporary col to compute the DR
    daily_stats_dendro$temp <- c(daily_stats_dendro$max[2:(nrow(daily_stats_dendro))], NA)
    
    daily_stats_dendro <- daily_stats_dendro %>%
      mutate(DR = temp - min, DG = temp -  max) %>%
      mutate(CumGrowth = cumsum(DR)) %>% 
      mutate(Date = unique(dendro_df$Date)) %>% 
      dplyr::select(base, dendro, Date, DOY, min, max, MDS, DG, DR, CumGrowth)
    
    all_stats <- rbind(all_stats, daily_stats_dendro)
  } else {
    # Adding maximum daily shrinkage
    daily_stats_dendro <- daily_stats_dendro %>%
      mutate(MDS = max - min, dendro = dendro, base = base)
    
    daily_stats_dendro <- daily_stats_dendro %>%
      mutate(DR = NA, DG = NA) %>%
      mutate(CumGrowth = NA) %>% 
      mutate(Date = unique(dendro_df$Date)) %>% 
      dplyr::select(base, dendro, Date, DOY, min, max, MDS, DG, DR, CumGrowth)
    
    all_stats <- rbind(all_stats, daily_stats_dendro)
  }
  
}


write.csv(x = as.data.frame(all_stats),
          file = file.path(odir, 'all_stats.csv'),
          row.names = FALSE)


#############################################################################################
##########PART F: Compute all the Stem Cycle stats and save them in the folder Results ######
#############################################################################################

idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/6-Jumps_fixed_partitioned_no_gaps"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Results"

files <- list.files(idir, pattern = ".csv")
all_sc_cycle <- NULL
all_sc_phase <- NULL
file <- files[1]
for(file in files){
  i <- i + 1
  print(paste(
    "Processing file",
    file,
    "Code",
    i
  ))
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  # dendro_df <- dendro_df  %>% distinct(Time, .keep_all = TRUE)
  # dendro_df <- spline.interpolation(dendro_df, resolution = 15, fill = TRUE)
  # dendro_df <- dendro_df[c(1,3)]
  # dendro_df <- dendro_df[1:(nrow(dendro_df) - 20),]
  # colnames(dendro_df) <- c('Time', 'T2')
  
  dendro_df$Date <- date(dendro_df$Time)
  day_s = yday(dendro_df$Date[1])
  day_s
  day_e =yday(last(dendro_df$Date))
  day_e
  
  if(day_e != day_s){
    s <- strsplit(file, '_')
    base <- s[[1]][4]
    dendro <- s[[1]][6]
    
    sc_stats <- phase.sc(dendro_df,
                         TreeNum = 1,
                         smoothing = 1,
                         outputplot = FALSE,
                         days = c(day_s, day_e))
    
    sc_cycle_df <- data.frame(
      phases = sc_stats$SC_cycle$Phases,
      start = sc_stats$SC_cycle$start,
      end = sc_stats$SC_cycle$end,
      Duration_m = sc_stats$SC_cycle$Duration_m,
      Duration_h = sc_stats$SC_cycle$Duration_h,
      magnitude = sc_stats$SC_cycle$magnitude,
      rate = sc_stats$SC_cycle$rate,
      DOY = sc_stats$SC_cycle$DOY
    )
    
    
    sc_cycle_df$base <- base
    sc_cycle_df$dendro <- dendro
    
    sc_phase_df <- data.frame(
      Time = sc_stats$SC_phase$Time,
      Phases = sc_stats$SC_phase$Phases
    )
    
    sc_phase_df$base <- base
    sc_phase_df$dendro <- dendro
    
  }
  
  all_sc_cycle <- rbind(all_sc_cycle, sc_cycle_df)
  all_sc_phase <- rbind(all_sc_phase, sc_phase_df)
  
}

write.csv(x = as.data.frame(all_sc_cycle),
          file = file.path(odir, 'all_sc_cycle.csv'),
          row.names = FALSE)

write.csv(x = as.data.frame(all_sc_phase),
          file = file.path(odir, 'all_sc_phase.csv'), 
          row.names = FALSE)


#############################################################################################
##########PART G: Compute all theZero Growth  stats and save them in the folder Results #####
#############################################################################################

idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/6-Jumps_fixed_partitioned_no_gaps"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Results"

files <- list.files(idir, pattern = ".csv")
all_zg_cycle <- NULL
all_zg_phase <- NULL
i <- 0
good_files <-  NULL
bad_files <- NULL

for(file in files){
  i <- i + 1
  print(paste(
    "Processing file",
    file,
    "Code",
    i
  ))
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")

  #Use to remove data after 21:45
  # last_sample_time <- as.POSIXct(last(date(dendro_df$Time)))
  # hour(last_sample_time) <- 21
  # minute(last_sample_time) <- 45
  # dendro_df <- dendro_df %>% 
  #   filter(Time <= last_sample_time)
  
  
  dendro_df$Date <- date(dendro_df$Time)
  day_s = yday(dendro_df$Date[1])
  day_s
  day_e =yday(last(dendro_df$Date))
  day_e
  
  
  
  if(day_e != day_s){
    s <- strsplit(file, '_')
    base <- s[[1]][4]
    dendro <- s[[1]][6]
    
    res <- try(zg_stats <- phase.zg(dendro_df[1:(nrow(dendro_df)-7), c(1,2)],
                                    TreeNum = 1,
                                    outputplot = FALSE,
                                    days = c(day_s, day_e)))
    if(inherits(res, "try-error")) #Report the bad files
    {
      bad_files <- rbind(bad_files,
                         data.frame(
                           Name = file,
                           start_date = dendro_df$Time[1],
                           end_date = last(dendro_df$Time),
                           start_val = dendro_df$T2[1],
                           end_val = last(dendro_df$T2),
                           nrow = nrow(dendro_df),
                           err_message = res[1]
                         ))
      next
    } else{ #If there is no error, bind the results together
      good_files <- rbind(good_files,
                         data.frame(
                           Name = file,
                           start_date = dendro_df$Time[1],
                           end_date = last(dendro_df$Time),
                           start_val = dendro_df$T2[1],
                           end_val = last(dendro_df$T2),
                           nrow = nrow(dendro_df)
                  ))
      zg_cycle_df <- data.frame(
        DOY = zg_stats$ZG_cycle$DOY,
        Phases = zg_stats$ZG_cycle$Phases,
        start = zg_stats$ZG_cycle$start,
        end = zg_stats$ZG_cycle$end,
        Duration_h = zg_stats$ZG_cycle$Duration_h,
        magnitude = zg_stats$ZG_cycle$magnitude,
        rate = zg_stats$ZG_cycle$rate,
        Max.twd = zg_stats$ZG_cycle$Max.twd,
        Max.twd.time = zg_stats$ZG_cycle$Max.twd.time,
        Avg.twd = zg_stats$ZG_cycle$Avg.twd,
        STD.twd = zg_stats$ZG_cycle$STD.twd
      )
      
      zg_phase_df <- data.frame(
        Time = zg_stats$ZG_phase$Time,
        Phases = zg_stats$ZG_phase$Phases,
        TWD = zg_stats$ZG_phase$TWD
      )
      
      all_zg_cycle <- rbind(all_zg_cycle, zg_cycle_df)
      all_zg_phase <- rbind(all_zg_phase, zg_phase_df)
    }
  }
}


write.csv(x = as.data.frame(all_zg_cycle),
          file = file.path(odir, 'all_zg_cycle.csv'),
          row.names = FALSE)

good_files <- good_files %>% 
  separate(start_date, into = c('sdate', 'stime'), sep = 10) %>% 
  separate(end_date, into = c('edate', 'etime'), sep = 10) %>% 
  mutate(n_days = nrow/96)

write.csv(x = as.data.frame(good_files),
          file = file.path(odir, 'files_that_did_not_give_an_error_with_zg_cycle.csv'), 
          row.names = FALSE)

bad_files <- bad_files %>% 
  separate(start_date, into = c('sdate', 'stime'), sep = 10) %>% 
  separate(end_date, into = c('edate', 'etime'), sep = 10) %>% 
  mutate(n_days = nrow/96) %>% 
  arrange(err_message)

write.csv(x = as.data.frame(bad_files),
          file = file.path(odir, 'files_that_gave_an_error_with_zg_cycle_removed_before.csv'),
          row.names = FALSE)


#############################################################################################
##########PART X:  Support code to investigate the problem due to ZG Cycle ##################
#############################################################################################
idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Results"
 
good_file<- 'files_that_did_not_give_an_error_with_zg_cycle.csv'
bad_file <- "files_that_gave_an_error_with_zg_cycle.csv"
good_file_minus_last_7 <- 'files_that_did_not_give_an_error_with_zg_cycle_removed_data_after_21_45.csv'
bad_file_minus_last_7 <- "files_that_gave_an_error_with_zg_cycle_removed_data_after_21_45.csv"

good <- read.csv(file.path(idir, 'files_that_did_not_give_an_error_with_zg_cycle.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
bad <- read.csv(file.path(idir, 'files_that_gave_an_error_with_zg_cycle.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
good_7 <- read.csv(file.path(idir, 'files_that_did_not_give_an_error_with_zg_cycle_removed_data_after_21_45.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)
bad_7 <- read.csv(file.path(idir, 'files_that_gave_an_error_with_zg_cycle_removed_data_after_21_45.csv'), header = TRUE, sep = ",", stringsAsFactors = FALSE)

length(intersect(good$Name, good_7$Name))
length(intersect(bad$Name, bad_7$Name))

#############################################################################################
##########PART Y: #Support function to know the number of days in each files#################
#############################################################################################


files <- list.files(idir, pattern = ".csv")

two_na <- files[c(13, 31, 76, 105)]
shorter <- files[c(21, 39, 48, 55, 64, 65, 73, 77, 83, 102, 110, 114, 115, 117, 122, 139, 140, 144,
                   150, 158, 168, 169, 190, 213, 218)]

days <- NULL
sizes <- NULL
i <- 1
for(file in files){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  # print(paste(
  #   "File",
  #   file,
  #   "contains",
  #   length(unique(date(dendro_df$Time))),
  #   "days"
  # ))
  days[i] <- length(unique(date(dendro_df$Time)))
  sizes[i] <- nrow(dendro_df)
  i <- i + 1
  
}
