library(lubridate)
require(lubridate)
# install.packages("dplyr")                       
library("dplyr")  
require("dplyr")
# install.packages("tapply") 
library('tidyr')
library(ggplot2)


setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/02-original_dendro_data/Paper_1")

Tree_data_dendro <- read.table("Tree_data_dendro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE,  encoding = "latin1")
head(Tree_data_dendro)
str(Tree_data_dendro)

Tree_data_dendro$Date <- as.Date(Tree_data_dendro$Date, format = "%d.%m.%Y")
Tree_data_dendro <- Tree_data_dendro %>% rename(Dendro = Label)

# Trying a better way to do the graphs about the abortion size_________________________________________________________________________________

pcsize <- pivot_longer(Tree_data_dendro, cols=c(5:8), names_to = "PCSize", values_to = "Value",names_repair = "unique") 


pcsize<- as.data.frame(pcsize)
Tree_data_dendro <- pcsize
Tree_data_dendro$PCSize <- as.factor(Tree_data_dendro$PCSize)
str(Tree_data_dendro)
head(Tree_data_dendro)


p1 <- ggplot(Tree_data_dendro,aes(x=Label, y=Value, fill=PCSize)) +
  geom_bar(stat="identity", position = "dodge") 

p1 + ylim(0, 20)
#geom_text(aes(label = Value...51, vjust = -0.2))  



# Add "year" and "weeks" column____________________________________________________________________________________________________________________________________________________________
Tree_data_dendro["Week"] <- Tree_data_dendro$Date
Tree_data_dendro$Week <- week(Tree_data_dendro$Week)
head(Tree_data_dendro)
Tree_data_dendro["Year"] <- Tree_data_dendro$Date
head(Tree_data_dendro)
str(Tree_data_dendro)
Year <- as.Date(Tree_data_dendro$Date,format = "%Y")
format(Year,'%Y')
Tree_data_dendro$Year <- as.numeric(format(Year,'%Y'))
head(Tree_data_dendro)


df <- Tree_data_dendro

#Define factors
df$Date                        <- as.Date     (df$Date, format ="%d.%m.%Y")
df$Flushes                     <- as.factor   (df$Flushes)
df$Flowers                     <- as.factor   (df$Flowers)
df$Dendro                      <- as.factor   (df$Dendro)
df$Linie                       <- as.factor  (df$Linie)
df$Plant                       <- as.factor  (df$Plant)
df$Week                        <- as.integer  (df$Week)
df$Year                        <- as.integer  (df$Year)
df$PC2.5cm                     <- as.numeric   (df$PC2.5cm)
df$PC5cm                       <- as.numeric  (df$PC5cm)
df$PC7.5cm                     <- as.numeric  (df$PC7.5cm)
df$PC10cm                      <- as.numeric  (df$PC10cm)
df$Fruit                       <- as.numeric  (df$Fruit)
df$TotalPC                     <- as.numeric  (df$TotalPC)


summary(df)
head(df)
str(df)


setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R/data_v3")
dendro <- read.table("Dendro_dataset_v4.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
head(dendro)
head(df)
dendro$Date <- as.Date(dendro$Date  , format = "%d.%m.%Y")
str(dendro)
df
str(dendro)

colnames(df)[colnames(df) == 'Label'] <- 'Dendro'
dendro$Date.x <- NULL
#df<- rename(df, Date= Date.x)
df2 <- merge(dendro, df, by= c("Dendro","Week", "Year"), all =TRUE)
head(df2)
str(df2)

write.csv(df2, file = "df2_v2.csv")



# Data format

pcsize <- pivot_longer(df2, cols=c(46:49), names_to = "PCSize", values_to = "Value",names_repair = "unique") 

  
pcsize<- as.data.frame(pcsize)
df2 <- pcsize
df2$PCSize <- as.factor   (df2$PCSize)
str(df2)
head(df2)



# Other data from HR

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/02-original_dendro_data/Paper_1")
clima_day <- read.table("clima_day.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE,  encoding = "latin1")
head(clima_day)
str(clima_day)

clima_day$Date <-  as.Date(clima_day$Date , format = "%d.%m.%Y")
head(dendro)
df2$Date.x <-  as.Date(df2$Date.x , format = "%d.%m.%Y")
colnames(df2)[colnames(df2) == 'Date.x'] <- 'Date'
str(df2)
head(df2)


df3 <- merge(df2,clima_day, by= "Date", all.x= TRUE)
setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R/data_v3")
write.csv(df3, file = "df3_v3.csv")

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R/data_v3")
df3 <- read.table("df3_v3.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
head(df3)
head(df3)


df3$Date <-  as.Date(df3$Date , format = "%d.%m.%Y")
head(df3)
str(df3)

#colnames(df3)[colnames(df3) == 'Date.x'] <- 'Date'
#Data <- Data[,-2]
#df4 <- merge(clima_day, df3, by.y = "Date", all.y =TRUE)



#Define factors

df$Date                        <- as.Date(df$Date, format ="%d.%m.%Y")
df3$Flushes.x                     <- as.factor(df3$Flushes.x)
df$Median20cm                  <- as.numeric(df$Median20cm)
df$Media40cm                   <- as.numeric  (df$Media40cm)
df3$Flowers.x                     <- as.factor   (df3$Flowers.x)
df$ET0_mm.j                    <- as.numeric  (df$ET0_mm.j)
df$Acc_PLUVIO                  <- as.numeric  (df$Acc_PLUVIO)
df$Dendro                      <- as.factor   (df$Dendro)
df$DG                          <- as.numeric  (df$DG)
df$MDS                         <- as.numeric  (df$MDS)
df$max                         <- as.numeric  (df$max)
df$min                         <- as.numeric  (df$min)
df$Type                        <- as.factor   (df$Type)
df$RADIATIONSOLAIRE            <- as.numeric  (df$RADIATIONSOLAIRE)
df$Windmaxm.s                  <- as.numeric  (df$Windmaxm.s)
df$MaxTemp                     <- as.numeric  (df$MaxTemp)
df$Micro.climate.x             <- as.factor   (df$Micro.climate.x)
df$Irrigation                  <- as.factor   (df$Irrigation)
df$minVPD_kPa                  <- as.numeric  (df$minVPD_kPa)
df$maxVPD_kPa                  <- as.numeric  (df$maxVPD_kPa)


# Some graphs_____________________________________________________________________________________________________________________________________

par (mfrow= c (1, 1))

bstats <- boxplot(df3$maxVPD_kPa ~ df3$Flushes.x, data = df3, xlab="Flushes.x", ylab="DR", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="Vapour-pressure deficit")  # this will plot without any outlier points

bstats <- boxplot(df$HR...y ~ df$Flowers.x , data = df, xlab="Flowers.x", ylab="Abortion", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flowers", ylab="Cumulative growth")  # this will plot without any outlier points

bstats <- boxplot(df$ETP ~ df$Flushes.x , data = df, xlab="Flushes.x", ylab="TotalPC", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes.x", ylab="MDS")  # this will plot without any outlier points

# Another way_________________________________________________________________________________________________________________________________

library(ggplot2)
library(viridis)
library(hrbrthemes)

ggplot(df3, aes(fill=PCSize, y=Value.y, x=Flushes.x)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Abortion size...") +
  facet_wrap(~Flushes.x) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

# Another way____________________________________________________________________________________________________________________________________

# library
library(ggplot2)
library(viridis)
library(hrbrthemes)

# Small multiple
ggplot(df3, aes(fill=PCSize, y=Value.y, x=Flushes.x), na.rm= FALSE) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Abortion size...") +
  theme_ipsum() +
  xlab("")


# Stacked
#attach(df3$Value...51)
ggplot(df3, aes(fill=PCSize, y=Value.y, x=Flushes.x)) + 
  geom_bar(stat="identity")+
  xlab("Flush intensity")+
  ylab("Unit")
  
  #geom_text(aes(label = Value...51), position= position_stack(vjust = 1.5), colour = "white")

# Another way____________________________________________________________________________________________________________________________________

ggplot(data = df3, aes(Flushes.x, Value.y, group = PCSize)) +
  geom_col(aes(fill = PCSize)) +
  geom_text(aes(label = Value.y), position = position_stack(vjust = 0.5))
  
  # Another way____________________________________________________________________________________________________________________________________

ggplot(df3, aes(fill=PCSize, y=Value.y, x=Flushes.x)) + 
  geom_col(position="dodge")+
  geom_text(aes(label = df3$Value.y), vjust = 1.5, colour = "white")
           
           

#ploting a bar plot for the different Flushes.x where the count is Value..51 and the fill is PCSize

# Another way____________________________________________________________________________________________________________________________________

table(complete.cases(df3))
df3 <- df3[complete.cases(df3),]
library(ggplot2)
library(reshape2)

df4 <- melt(df3 ,  id.vars = 'time', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df4, aes(time,value)) + geom_line(aes(colour = series))

# or plot on different plots
ggplot(df4, aes(time,value)) + geom_line() + facet_grid(series ~ .)
# Another way____________________________________________________________________________________________________________________________________

plot(df3$Flushes.x ~ df3$PCSize, data=df3,
     pch= as.integer(Value...51),
     col= Value...51)

# Another way____________________________________________________________________________________________________________________________________

p1 <- ggplot(df3,aes(x=Flushes.x, y=Value...51, fill=PCSize),na.rm=FALSE) +
  geom_bar(stat="identity", position = "dodge") 
   
p1 + ylim(0, 20)
#geom_text(aes(label = Value...51, vjust = -0.2))  

# Another way____________________________________________________________________________________________________________________________________

plot(x=df3$Flushes.x, y= df3$Value.y)
+ 







