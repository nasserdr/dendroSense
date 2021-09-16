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
##########PART A1: Gather all dendros of each base in one datafram ##########################
#############################################################################################
idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-B-Jumps_fixed"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-A-Data_per_base"

files <- list.files(idir, pattern = '*.csv')

fs <- strsplit(files, "_")
fs_df <- data.table::transpose(as.data.frame(fs))

couples <- fs_df %>% select(4,6)
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
    select(Time, T2)
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

#Creating a reference dendrometer
# file_ref <- "3-B-Jumps_fixed/Jumps_fixed_Base_1811_Dendro_531_Filtered.csv"
# ref_dend <- read.csv(file.path("~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/"
#                                , file_ref), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# colnames(ref_dend) <- c('Time', 'T2')
# ref_dend$Time <- as.POSIXct(ref_dend$Time, format = "%Y-%m-%d %H:%M:%OS")

files <- list.files(idir, pattern = ".csv")
file <- files[1]

for(file in files){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS", tz= 'CET')
  
  start <- lubridate::date(dendro_df$Time[1])
  end <- date(last(dendro_df$Time))

  #Why I am having indices bigger than one hour?
  indices <- zoo(NA,
                 seq(as.POSIXct(start, tz = 'CET'),
                     as.POSIXct(end, tz = 'CET'),by="15 min"))
  # df_ref <- data.frame(
  #   Time = indices,
  #   T2 = NA)
  
  df_ref$Time <- rownames(df_ref)
  rownames(df_ref) <- NULL
  df_ref$Time <- as.POSIXct(df_ref$Time, format = "%Y-%m-%d %H:%M:%OS")
  
  new_dendr <- merge(dendro_df, df_ref, by = "Time", all.x = TRUE)

  colnames(new_dendr) <- c('Time', 'T2')
  
  dendro_df_filled <- spline.interpolation(new_dendr, 15 , fill = FALSE)
  
  # dendro_df_filled %>% group_by(Date) %>% tally()
  
  # df1_NI<-network.interpolation(df=dendro_df, referenceDF=ref_dend, niMethod='linear')
  
  }


#############################################################################################
##########PART E: Compute all the stats and save them in the folder Results #################
#############################################################################################

idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/5-Jumps_fixed_partitioned"
odir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Results"

files <- list.files(idir, pattern = ".csv")
all_stats <- NULL
all_sc_cycle <- NULL
all_sc_phase <- NULL
all_zg_cycle <- NULL
all_zg_phase <- NULL
# out_without_removing_last_points <- 21, 30  31  39  42  48  55  64  65  73  76  77  81  83 102 105 110 114 115 117 118 122 139 140 144 150 158 168 169 190 213 218 229 230 235 242 244 245 246 247
# [41] 249 266 273 275 279 280 299 310 311 312 314 318 337 342 360 370 371 408 412 429 462 471 490 491 492 493 495 506 519 520 528 541 547 549 556 561 568 571 583 599
# [81] 603 606 607 609 627 628 630 635 636 655 668 679 704 715 716 725 732 735 739 740 755 764 765 766 780 804 820 832 837 839 841 844 851 854 863 865 866 869 871 872
# [121] 873 880 883 884 895 901 903 906

# out_removing_last_7 <- 7  21  31  48  64  76  77  94 102 105 114 117 129 150 154 156 200 204 209 214 229 234 245 247 249 250 266 271 273 274 275 278 280 299 310 311 312 313 315 342
# [41] 343 353 365 375 393 407 408 412 426 431 437 448 454 470 471 481 482 489 490 491 492 493 499 500 501 504 506 519 530 541 544 556 566 599 604 608 612 626 627 628
# [81] 630 634 635 656 676 683 704 714 715 723 725 732 735 740 763 764 769 772 773 774 775 801 804 805 807 823 837 838 841 847 850 851 858 859 866 869 871 873 880 884
# [121] 885 886 888 903
out <- NULL
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
  # dendro_df <- dendro_df[1:(nrow(dendro_df)-14),]
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format = "%Y-%m-%d %H:%M:%OS")
  dendro_df <- dendro_df  %>% distinct(Time, .keep_all = TRUE)
  dendro_df <- spline.interpolation(dendro_df, resolution = 15, fill = TRUE)
  dendro_df <- dendro_df[c(1,3)]
  dendro_df <- dendro_df[1:(nrow(dendro_df) - 20),]
  colnames(dendro_df) <- c('Time', 'T2')
  
  dendro_df$Date <- date(dendro_df$Time)
  day_s = yday(dendro_df$Date[1])
  day_s
  day_e =yday(last(dendro_df$Date))
  day_e
  
  if(day_e != day_s){
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
    
    res <- try(zg_stats <- phase.zg(dendro_df[c(1,2)],
                                    TreeNum = 1,
                                    outputplot = FALSE,
                                    days = c(day_s, day_e)))
    if(inherits(res, "try-error"))
    {
      out <- c(out, i)
      next
    }
    
    


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
    all_stats <- rbind(all_stats, daily_stats_dendro)
    all_sc_cycle <- rbind(all_sc_cycle, sc_cycle_df)
    all_sc_phase <- rbind(all_sc_phase, sc_phase_df)
    all_zg_cycle <- rbind(all_zg_cycle, zg_cycle_df)
    all_zg_phase <- rbind(all_zg_phase, zg_phase_df)
    
  }
}

out
write.csv(x = as.data.frame(all_stats),
          file = file.path(odir, 'all_stats.csv'),
          row.names = FALSE)

write.csv(x = as.data.frame(all_sc_cycle),
          file = file.path(odir, 'all_sc_cycle.csv'),
          row.names = FALSE)

write.csv(x = as.data.frame(all_sc_phase),
          file = file.path(odir, 'all_sc_phase.csv'), 
          row.names = FALSE)

write.csv(x = as.data.frame(all_zg_cycle),
          file = file.path(odir, 'all_zg_cycle.csv'),
          row.names = FALSE)

write.csv(x = as.data.frame(all_sc_phase),
          file = file.path(odir, 'all_zg_phase.csv'), 
          row.names = FALSE)

#Support function to know the number of days in each files
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
