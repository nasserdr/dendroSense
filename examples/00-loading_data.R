#This example is unfinished
library(readr)
library(dplyr)
library(data.table)
library(xts)

idir = "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Clean_signals"
files <- list.files(idir)
fs <- strsplit(files, "_")
fs_df <- data.table::transpose(as.data.frame(fs))
fs_df <- fs_df[-nrow(fs_df),]
dendros <- unique(fs_df$V2)

gather_all_days_for_one_dendro <- function(files_unique_dendro, idir){
  df <- NULL
  for(file in files_unique_dendro){
    df1 <- read_csv(file.path(idir, file))
    names(df1) <- c("dt", "val")
    df <- dplyr::bind_rows(df, df1)
  }
  
  return(df)
}

#First dendro data
files_unique_dendro <- files[grep(dendros[1], files)]

df <- gather_all_days_for_one_dendro(files_unique_dendro, idir)



df_xts <- as.xts(
  x = df$val,
  order.by = df$dt
)

#inspecting df
y <- df_xts[ ! duplicated( index(df_xts) ),  ]


plot.xts(y)

files_unique_dendro <- files[grep(dendros[2], files)]
dff <- gather_all_days_for_one_dendro(files_unique_dendro, idir)



r_xts <- rbind(df2_xts, df1_xts)
xts <- data.frame(date=index(r_xts), coredata(r_xts))

f = "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Clean_signals/Base_1797_Dendro_222_from_2017-02-14_to_2017-02-21_F1.csv"
df1 <- read_csv(f)
names(df1) <- c("dt", "val")
plot(df1$dt, df1$val)

f = "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/Clean_signals/Base_1797_Dendro_222_from_2017-02-10_to_2017-02-14_F1.csv"
df <- read_csv(f)
names(df) <- c("dt", "val")
plot(df$dt, df$val)

