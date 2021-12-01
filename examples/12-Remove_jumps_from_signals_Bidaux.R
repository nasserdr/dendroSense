#' This code reads the filtered signals (from Clean_signals_gathered), remove 
#' the jumps and then save the signals without jumps in Jumps_fixed
#' This code takes into account that the data come with a timezone of Brazil and 
#' therefore it correct the timezone of the data

library(readr)
library(lubridate)
library(dendRoAnalyst)
library(dendrometeR)
library(dplyr)
library(ggplot2)
library(data.table)
# -----------------------------------------------

# -----------------------------------------------

# shift the start by n time points (here as example 4, equivalent to 1 hour)
idir = "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/20210911_v4/dendro_nogaps15min"
files <- list.files(idir, pattern = '*.csv')
odir = "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/20210911_v4/dendro_noJump"

#Change the path file first
#Run the jumper
#Discover the max number of TreeNum
#Run as many times the jumper as the TreeNum

## RUN THIS PER FILE
file <- files[8]
files


dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ";", stringsAsFactors = FALSE)
dendro_df[,1] <- as.POSIXct(dendro_df[,1], format = "%Y-%m-%dT %H:%M:%OSZ")


#dendro_df <- na.omit(dendro_df)

df <- NULL
df <- data.frame(
  date = dendro_df[,1] #Here we will merge all the jump fixed dendros
)

head(dendro_df)
tail(dendro_df)
ncol(dendro_df) - 1

## RUN THIS PER DENDRO
tree_num <- 3
plot(x = dendro_df[,1], y = dendro_df[,tree_num+1], type = 'l', xlab = 'Time', ylab = names(dendro_df)[tree_num+1])
v <- 100

df_jumps <- jump.locator(dendro_df, TreeNum = tree_num, v = v)

df_jumps[,1] <- as.POSIXct(df_jumps[,1], format = "%Y-%m-%d %H:%M:%OS")


## RUN THIS PER FILE (ONCE ALL DENDROS ARE DONE)

#No jumps
readr::write_csv(df_jumps, file.path(odir, paste0('No_jumps_', names(df_jumps)[tree_num +1], '_',file)))

#with jumps 
readr::write_csv(df_jumps, file.path(odir, paste0('No_jumps_', names(df_jumps)[2], '_',file)))

