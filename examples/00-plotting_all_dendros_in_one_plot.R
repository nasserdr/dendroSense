library(xts)

idir <- "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/3-B-Jumps_fixed"
dendro_2021 <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)

all_xts <- NULL

for(file in files){
  dendro_df <- read.csv(file.path(idir, file), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  colnames(dendro_df) <- c('Time', 'T2')
  dendro_df$Time <- as.POSIXct(dendro_df$Time, format ="%Y-%m-%d %H:%M:%OS")
  #dendro_df$Time <-as.Date(dendro_df$Time>= as.Date("2017-01-01") & dendro_df$Time <= as.Date("2017-07-31"))
    dendro_xts <- xts(
    x = dendro_df$T2,
    order.by = dendro_df$Time
  )
  
  dendro_xts <- dendro_xts["20170101/20170630"]
  all_xts <- xts::merge.xts(all_xts, dendro_xts)
}

plot.xts(all_xts)
names <- unlist(lapply(files, function(x){substr(x, 13, 32)}))
names(all_xts) <- names
plot.xts(all_xts, legend.loc = c(0.8, 0.8))

#############################################################################################
#############################################################################################
#############################################################################################

df <- data.frame(
  time = index(all_xts),
  as.data.frame(all_xts))
row.names(df) <- NULL


library(ggplot2)

library(reshape2)

df <- melt(df ,  id.vars = 'time', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(time,value)) + geom_line(aes(colour = series))

# or plot on different plots
ggplot(df, aes(time,value)) + geom_line() + facet_grid(series ~ .)

