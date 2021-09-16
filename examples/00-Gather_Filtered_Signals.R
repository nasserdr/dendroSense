#' This code reads all the (parts of) signals from excel files, identify to which dendro
#' they belong, merges all the files together and then save them in Clean_signals_gathered.
#' This code does not change the time zone of the data, so, the data produced are with a
#' time zone that is wrong. The time zone should be changed in subsequent codes

library(readr)
library(dplyr)
library(data.table)
library(xts)
library(ggplot2)

# Listing all the files inside clean_signals 
idir = "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/1-Clean_signals"
odir = "~/mnt/agroscope/Data-Work/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Hassan/2-Clean_signals_gathered"

files <- list.files(idir, pattern = '*.csv')
fs <- strsplit(files, "_")
fs_df <- data.table::transpose(as.data.frame(fs))

couples <- fs_df %>% select(2,4)
couples <- distinct(couples)
names(couples) <- c('base', 'dendro')
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
for (i in 1:nrow(couples)){
  base <- couples[i, 1]
  dendro <- couples[i, 2]
  files_unique_base <- files[grep(base, files)]
  files_unique_dendro <- files_unique_base[grep(dendro, files_unique_base)]
  df <- gather_all_days_for_one_dendro(files_unique_dendro, idir)
  df_xts <- as.xts(
    x = df$val,
    order.by = df$dt
  )
  y <- df_xts[ ! duplicated( index(df_xts) ),  ]
  y <- data.frame(
    datetime = index(y),
    value = coredata(y)
  )
  names(y) <- c('datetime', 'value')
  
  
  write.csv(x = as.data.frame(y),
            file = file.path(odir, paste0('Base_', base, '_Dendro_', dendro, '_Filtered.csv')),
            row.names = FALSE)
  
  
  p <- ggplot(data = y, 
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
    filename = file.path(odir, paste0('Base_', base, '_Dendro_', dendro, '_Filtered.tiff')),
    plot = p,
    device = 'tiff',
    width = 16,
    height = 7,
    units = "cm",
    dpi = 400
  )
}
