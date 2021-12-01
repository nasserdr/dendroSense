

# Some analyzes____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________
library(lubridate)

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R")
dendro <- read.table("dendro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
head(dendro)
str(dendro)
 

y <- Dendro_dataset_v4 %>%
  group_by(Year)

y <- split(y, f = y$Year)
y_2017 <- y$`2017`
y_2018 <- y$`2018`

df <- dendro

df$Dendro <- as.factor(df$Dendro)
str(df)
summary(df$dendro)


#Flushes x MDS____________________________________________________________________________________________________________________________________________________________________

par (mfrow= c (1, 1))
plot(df$MDS ~ df$Flushes , pch=20, xlab="Flushes", ylab="MDS", col="darkblue")
abline(lm(df$MDS ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$MDS ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

cor(df$MDS, df$Flushes)
bstats <- boxplot(df$MDS ~ df$Flushes, data = df,xlab="Flushes", ylab="MDS", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="MDS")  # this will plot without any outlier points


#Flushes x Daily Recovery____________________________________________________________________________________________________________________________________________________________________

par (mfrow= c (1, 1))
plot(df$DR ~ df$Flushes , pch=20, xlab="Flushes", ylab="DR", col="darkblue")
abline(lm(df$DR ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$DR ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

cor(df$DR, df$Flushes)
bstats <- boxplot(df$DR ~ df$Flushes, data = df,xlab="Flushes", ylab="DR", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="DR")  # this will plot without any outlier points

#Flushes x Daily Growth____________________________________________________________________________________________________________________________________________________________________

par (mfrow= c (1, 1))
plot(df$DG ~ df$Flushes , pch=20, xlab="Flushes", ylab="DG", col="darkblue")
abline(lm(df$DG ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$DG ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

cor(df$DG, df$Flushes)
bstats <- boxplot(df$DG ~ df$Flushes, data = df,xlab="Flushes", ylab="DG", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="DG")  # this will plot without any outlier points


#Flushes x Acummulativ growth____________________________________________________________________________________________________________________________________________________________________

par (mfrow= c (1, 1))
plot(df$CumGrowth ~ df$Flushes , pch=20, xlab="Flushes", ylab="CumGrowth", col="darkblue")
abline(lm(df$CumGrowth ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$CumGrowth ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

cor(df$CumGrowth, df$Flushes)
bstats <- boxplot(df$CumGrowth ~ df$Flushes, data = df,xlab="Flushes", ylab="CumGrowth", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="CumGrowth")  # this will plot without any outlier points


#Flushes x Abortion____________________________________________________________________________________________________________________________________________________________________

plot(df$Abortion ~ df$Flushes , pch=20, xlab="Flushes", ylab="Abortion", col="darkblue")
abline(lm(df$Abortion ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$df$Abortion ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

bstats <- boxplot(df$Abortion ~ df$Flushes , data = df, xlab="Flushes", ylab="Abortion", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="Abortion")  # this will plot without any outlier points

plot(df$Abortion ~ df$Flushes , pch=20, xlab="Flushes", ylab="Abortion", col="darkblue")
abline(lm(df$Abortion ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$df$Abortion ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Flushes x Healthy fruits____________________________________________________________________________________________________________________________________________________________________

plot(df$HealthyFruits ~ df$Flushes , pch=20, xlab="Flushes", ylab="HealthyFruits", col="darkblue")
abline(lm(df$HealthyFruits ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$df$HealthyFruits ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

bstats <- boxplot(df$HealthyFruits ~ df$Flushes , data = df, xlab="Flushes", ylab="HealthyFruits", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="HealthyFruits")  # this will plot without any outlier points

plot(df$HealthyFruits ~ df$Flushes , pch=20, xlab="Flushes", ylab="HealthyFruits", col="darkblue")
abline(lm(df$HealthyFruits ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$df$HealthyFruits ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Flushes x DPV____________________________________________________________________________________________________________________________________________________________________

plot(df$Acc_PLUVIO ~ df$Flushes , pch=20, xlab="Flushes", ylab="Acc_PLUVIO", col="darkblue")
abline(lm(df$Acc_PLUVIO ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$df$Acc_PLUVIO ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

bstats <- boxplot(df$Acc_PLUVIO ~ df$Flushes , data = df, xlab="Flushes", ylab="Acc_PLUVIO", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="Acc_PLUVIO")  # this will plot without any outlier points

plot(df$Acc_PLUVIO ~ df$Flushes , pch=20, xlab="Flushes", ylab="Acc_PLUVIO", col="darkblue")
abline(lm(df$Acc_PLUVIO ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$df$Acc_PLUVIO ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 


#Flushes x MDS____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(y_2017$MDS ~ y_2017$Flushes, data = df,xlab="Flushes", ylab="MDS2017 ", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="MDS2017 ")  # this will plot without any outlier points


boxplot(y_2017$MDS ~ y_2017$Flushes)
cor(y_2017$MDS, y_2017$Flushes)
plot(y_2017$MDS ~ y_2017$Flushes, pch=20, xlab="Flushes", ylab="MDS2017 ", col="darkblue")
abline(lm(y_2017$MDS ~ y_2017$Flushes) , lwd=2, col="red")
rg <- lm(y_2017$MDS ~ y_2017$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Flowers x HR..____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$Median20cm ~ df$Flowers, data = df,xlab="Flowers", ylab="Median20cm", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flowers", ylab="Median20cm")  # this will plot without any outlier points


boxplot(df$Median20cm ~ df$Flowers)
cor(df$Median20cm ,df$Flowers)
plot(df$Median20cm ~ df$Flowers, pch=20, xlab="Flowers", ylab="Median20cm ", col="darkblue")
abline(lm(df$Median20cm  ~ df$Flowers) , lwd=2, col="red")
rg <- lm(df$Median20cm  ~ df$Flowers)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Flowers x MDS____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$HR.. ~ df$Flowers, data = df,xlab="Flowers", ylab="HR..", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flowers", ylab="HR..")  # this will plot without any outlier points


boxplot(df$MDS  ~ df$Flowers)
cor(df$MDS ,df$Flowers)
plot(df$MDS ~ df$Flowers, pch=20, xlab="Flowers", ylab="MDS", col="darkblue")
abline(lm(df$MDS  ~ df$Flowers) , lwd=2, col="red")
rg <- lm(df$MDS ~ df$Flowers)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 


#Flowers x Abortion____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$meanVPD_kPa ~ df$Flushes, data = df,xlab="Flushes", ylab="meanVPD_kPa", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flushes", ylab="meanVPD_kPa")  # this will plot without any outlier points


boxplot(df$meanVPD_kPa  ~ df$Flushes)
cor(df$meanVPD_kPa ,df$Flushes)
plot(df$meanVPD_kPa ~ df$Flushes, pch=20, xlab="Flushes", ylab="meanVPD_kPa", col="darkblue")
abline(lm(df$meanVPD_kPa  ~ df$Flushes) , lwd=2, col="red")
rg <- lm(df$meanVPD_kPa ~ df$Flushes)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Flowers x Healthy fruits____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$HealthyFruits ~ df$Flowers, data = df,xlab="Flowers", ylab="HealthyFruits", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flowers", ylab="HealthyFruits")  # this will plot without any outlier points


boxplot(df$HealthyFruits  ~ df$Flowers)
cor(df$HealthyFruits ,df$Flowers)
plot(df$HealthyFruits ~ df$Flowers, pch=20, xlab="Flowers", ylab="HealthyFruits", col="darkblue")
abline(lm(df$HealthyFruits ~ df$Flowers) , lwd=2, col="red")
rg <- lm(df$HealthyFruits ~ df$Flowers)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Flowers x Abortion____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$Abortion ~ df$Flowers, data = df,xlab="Flowers", ylab="Abortion", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flowers", ylab="Abortion")  # this will plot without any outlier points


boxplot(df$Abortion  ~ df$Flowers)
cor(df$Abortion ,df$Flowers)
plot(df$Abortion ~ df$Flowers, pch=20, xlab="Flowers", ylab="Abortion", col="darkblue")
abline(lm(df$Abortion ~ df$Flowers) , lwd=2, col="red")
rg <- lm(df$Abortion ~ df$Flowers)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#VPD x MDS____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$MDS ~ df$maxVPD_kPa, data = df,xlab="maxVPD_kPa", ylab="MDS ", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="maxVPD_kPa", ylab="MDS ")  # this will plot without any outlier points


boxplot(df$MDS  ~ df$maxVPD_kPa)
cor(df$MDS  ,df$maxVPD_kPa)
plot(df$MDS  ~ df$maxVPD_kPa, pch=20, xlab="maxVPD_kPa", ylab="MDS ", col="darkblue")
abline(lm(df$MDS  ~ df$maxVPD_kPa) , lwd=2, col="red")
rg <- lm(df$MDS  ~ df$maxVPD_kPa)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#ETO x MDS____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$MDS ~ df$ET0_mm.j, data = df,xlab="ET0_mm.j", ylab="MDS ", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="ET0_mm.j", ylab="MDS ")  # this will plot without any outlier points


boxplot(df$MDS  ~ df$ET0_mm.j)
cor(df$MDS  ,df$ET0_mm.j)
plot(df$MDS  ~ df$ET0_mm.j, pch=20, xlab="ET0_mm.j", ylab="MDS ", col="darkblue")
abline(lm(df$MDS  ~ df$ET0_mm.j) , lwd=2, col="red")
rg <- lm(df$MDS  ~ df$ET0_mm.j)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Abortion x Temparature____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$Abortion ~  df$MaxTemp , data = df,xlab="MaxTemp", ylab="Abortion ", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="MaxTemp", ylab="Abortion ")  # this will plot without any outlier points


boxplot(df$Abortion ~  df$MaxTemp)
cor(df$Abortion,df$MaxTemp)
plot(df$Abortion ~  df$MaxTemp, pch=20, xlab="MaxTemp", ylab="Abortion", col="darkblue")
abline(lm(df$Abortion ~  df$MaxTemp) , lwd=2, col="red")

rg <- lm(df$Abortion ~  df$MaxTemp)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Abortion x DG____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$Abortion ~  df$DG , data = df,xlab="DG", ylab="Abortion ", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="DG", ylab="Abortion ")  # this will plot without any outlier points


boxplot(df$Abortion ~  df$DG)
cor(df$Abortion,df$DG)
plot(df$Abortion ~  df$DG, pch=20, xlab="DG", ylab="Abortion", col="darkblue")
abline(lm(df$Abortion ~  df$DG) , lwd=2, col="red")

rg <- lm(df$Abortion ~  df$DG)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Abortion x HR____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$Abortion ~  df$HR.. , data = df,xlab="HR..", ylab="Abortion ", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="HR..", ylab="Abortion ")  # this will plot without any outlier points


boxplot(df$Abortion ~  df$HR..)
cor(df$Abortion,df$HR..)
plot(df$Abortion ~  df$HR.., pch=20, xlab="HR..", ylab="Abortion", col="darkblue")
abline(lm(df$Abortion ~  df$HR..) , lwd=2, col="red")

rg <- lm(df$Abortion ~  df$HR..)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 


#Abortion x Irrigation____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$Abortion ~  df$Irrigation, data = df,xlab="Irrigation", ylab="Abortion ", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Irrigation", ylab="Abortion")  # this will plot without any outlier points


boxplot(df$Abortion ~  df$Irrigation)
cor(df$Abortion,df$Irrigation)
plot(df$Abortion ~  df$Irrigation, pch=20, xlab="Irrigation", ylab="Abortion", col="darkblue")
abline(lm(df$Abortion ~  df$Irrigation) , lwd=2, col="red")

rg <- lm(df$Abortion ~  df$Irrigation)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Daily growth x Irrigation____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$DG ~  df$Irrigation, data = df,xlab="Irrigation", ylab="DG", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Irrigation", ylab="DG")  # this will plot without any outlier points


boxplot(df$DG ~  df$Irrigation)
cor(df$DG ,df$Irrigation)
plot(df$DG ~  df$Irrigation, pch=20, xlab="Irrigation", ylab="DG", col="darkblue")
abline(lm(df$DG ~  df$Irrigation) , lwd=2, col="red")

rg <- lm(df$DG ~  df$Irrigation)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#MSD x Irrigation____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$MDS ~  df$Irrigation, data = df,xlab="Irrigation", ylab="MDS", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Irrigation", ylab="MDS")  # this will plot without any outlier points


boxplot(df$MDS ~  df$Irrigation)
cor(df$MDS ,df$Irrigation)
plot(df$MDS ~  df$Irrigation, pch=20, xlab="Irrigation", ylab="MDS", col="darkblue")
abline(lm(df$MDS ~  df$Irrigation) , lwd=2, col="red")

rg <- lm(df$MDS ~  df$Irrigation)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 


head (df)


#Abortion x Micro.climate.x____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$DR ~  df$Dendro, data = df,xlab="Dendro", ylab="DR", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Dendro", ylab="DR")  # this will plot without any outlier points


boxplot(df$DR ~  df$Dendro)
cor(df$DR ,df$Dendro)
plot(df$DR ~  df$Dendro, pch=20, xlab="Dendro", ylab="DR", col="darkblue")
abline(lm(df$DR ~  df$Dendro) , lwd=2, col="red")

rg <- lm(df$Abortion ~  df$Abortion)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 


#MDS x Micro.climate.x____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$MDS ~  df$Micro.climate.x, data = df,xlab="Micro.climate.x", ylab="MDS", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Micro.climate.x", ylab="MDS")  # this will plot without any outlier points


boxplot(df$MDS ~  df$Micro.climate.x)
cor(df$MDS ,df$Micro.climate.x)
plot(df$MDS ~  df$Micro.climate.x, pch=20, xlab="Micro.climate.x", ylab="MDS", col="darkblue")
abline(lm(df$MDS ~  df$Irrigation) , lwd=2, col="red")

rg <- lm(df$MDS ~  df$Micro.climate.x)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#DR x Micro.climate.x____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$DR ~  df$Micro.climate.x, data = df,xlab="Micro.climate.x", ylab="DR", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Micro.climate.x", ylab="DR")  # this will plot without any outlier points


boxplot(df$DR ~  df$Micro.climate.x)
cor(df$DR ,df$Micro.climate.x)
plot(df$DR ~  df$Micro.climate.x, pch=20, xlab="Micro.climate.x", ylab="DR", col="darkblue")
abline(lm(df$DR ~  df$Irrigation) , lwd=2, col="red")

rg <- lm(df$DR ~  df$Micro.climate.x)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#DG x Micro.climate.x____________________________________________________________________________________________________________________________________________________________________

head(df)

bstats <- boxplot(df$CumGrowth ~  df$Dendro, data = df,xlab="Dendro", ylab="CumGrowth", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Dendro", ylab="CumGrowth")  # this will plot without any outlier points


boxplot(df$CumGrowth ~  df$Dendro)
cor(df$CumGrowth ,df$Dendro)
plot(df$CumGrowth ~  df$Dendro, pch=20, xlab="Dendro", ylab="CumGrowth", col="darkblue")
abline(lm(df$CumGrowth ~  df$Dendro) , lwd=2, col="red")

rg <- lm(df$CumGrowth ~  df$Dendro)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

#Flushes x irrigation____________________________________________________________________________________________________________________________________________________________________

par (mfrow= c (1, 1))
plot(df$RAYONNEMENTGLOBAL ~ df$Flushes , pch=20, xlab="RAYONNEMENTGLOBAL", ylab="Flushes", col="darkblue")
abline(lm(df$Flushes ~ df$RAYONNEMENTGLOBAL) , lwd=2, col="red")
rg <- lm(df$Flushes ~ df$RAYONNEMENTGLOBAL)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

cor(df$df$RAYONNEMENTGLOBAL ~ df$Flowers)
bstats <- boxplot(df$RAYONNEMENTGLOBAL ~ df$Flowers, data = df,xlab="Flowers", ylab="RAYONNEMENTGLOBAL", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="Flowers", ylab="RAYONNEMENTGLOBAL")  # this will plot without any outlier points


#Abortion x Median20cm____________________________________________________________________________________________________________________________________________________________________

par (mfrow= c (1, 1))
plot(df$Windmaxm.s ~ df$DG, pch=20, xlab="DG", ylab="Windmaxm.s", col="darkblue")
abline(lm(df$Windmaxm.s ~ df$DG) , lwd=2, col="red")
rg <- lm(df$Windmaxm.s ~ df$DG)
summary(rg)
predict(object = rg)
summary(rg)$r.squared 

cor(df$Windmaxm.s, df$DG)
bstats <- boxplot(df$Windmaxm.s ~ df$DG, data = df,xlab="DG", ylab="Windmaxm.s", col = "lightgray") 
#need to "waste" this plot
bstats$out <- NULL
bstats$group <- NULL
bxp(bstats,xlab="DG", ylab="Windmaxm.s")  # this will plot without any outlier points

library(ggplot2)
require(ggplot2)

qplot(df$Median20cm, df$Flowers)
stripchart(df$Irrigation~df$Flowers,data.frame(df$Irrigation,df$Flowers),pch=1,vertical=T)
plot(cut(Irrigation, 3) ~ group, data = df)

boxplot(Irrigation~Flowers, data= df)

lapply(df$Irrigation,classe)



ggplot(df$Irrigation, df$Flowers)
ggplot(df) +
  geom_point(aes(Irrigation, Flowers)) 
geom_point(data = df, aes(gp, mean), colour = 'red', size = 3)

head (df)

str(df)

range(df$Abortion)




#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Due to the reduction of data, these parameters are analyzed separately!!!!!!!!

# irrigation  ____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________
setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/02-original_dendro_data/Paper_1")
irrigation_dendro <- read.table("irrigation_dendro.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
head(irrigation_dendro)
str(irrigation_dendro)

irrigation_dendro$Date <- as.Date(irrigation_dendro$Date, format = "%d.%m.%Y")

# Merge irrigation and dendro_dataset_v3____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________


dendro_dataset_v4 <- merge(dendro_dataset_v3,irrigation_dendro, by = "Date", all.x = TRUE)

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R")
write.csv(dendro_dataset_v4, file = "dendro_dataset_v4.csv")

# Harvest ____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________
setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/02-original_dendro_data/Paper_1")
Harvest <- read.table("harvest_d.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
head(Harvest)
str(Harvest)


Harvest$Date <- as.Date(Harvest$Date, format = "%d.%m.%Y")
# Harvest <- Harvest %>% rename(Dendro = Label)

Harvest["Week"] <- Harvest$Date
Harvest$Week <- week(Harvest$Week)
head(Harvest)
Harvest["Year"] <- Harvest$Date
head(Harvest)
str(Harvest)
Year <- as.Date(Harvest$Date,format = "%Y")
format(Year,'%Y')
Harvest$Year <- as.numeric(format(Year,'%Y'))
head(Harvest)


# Merge Harvest and dendro_dataset_v4____________________________________________________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________________________________________________________

dendro_dataset_v5 <- merge(Dendro_dataset_v4,Harvest, by = c("Dendro", "Week", "Year"), all.y= TRUE)
head(dendro_dataset_v5)

dendro_dataset_v5 <- na.omit(dendro_dataset_v5)

head(dendro_dataset_v5)

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R")
write.csv(dendro_dataset_v5, file = "Dendro_dataset_v5.csv")



dendro_dataset_v4$X <- NULL
dendro_dataset_v4$m <- NULL
dendro_dataset_v4$X.1 <- NULL
dendro_dataset_v4 <- dendro_dataset_v4[, -c(35:45)]
dendro_dataset_v4 <- dendro_dataset_v4 %>% rename(Date = Date.x)
head(dendro_dataset_v4)
str(dendro_dataset_v4)

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R")
write.csv(Dendro_dataset_v4, file = "Dendro_dataset_v4.csv")

# Harvest another option______________________________________________________________________________________________________________________________________

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/02-original_dendro_data/Paper_1")
Harvest_Yield <- read.table("Harvest_Yield.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
head(Harvest_Yield)
str(Harvest_Yield)

Harvest_Yield <- Harvest_Yield %>% rename(Dendro = Label)
harv_dendro <- Harvest_Yield %>% group_by(Dendro)%>% 
  summarise_at(c("Total.weight..kg..per.10.Trees", "Yield.dry.beans..grams..per.kg.of.fruit.per.10.trees", "Yield.beans.dry.per.hectare..kg."), sum, na.rm = TRUE)



## Summe of all Fruits and abortion per tree_______________________________________________________________________________________________________________________________________
d_191 <- df[df[, "Dendro"] == "191", ]

sum_dendro <- df %>% group_by(Dendro)%>% 
  summarise_at(c("Abortion", "HealthyFruits"), sum, na.rm = TRUE)

head(dendro_dataset_v5)
dendro_dataset_v5 <- dendro_dataset_v5[, -51]


sum_all_dendro <- merge(sum_dendro,harv_dendro, by = "Dendro", all.x = TRUE)

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R")
write.csv(sum_all_dendro, file = "sum_all_dendro.csv")


#Plots ___________________________________________________________________________________________________________________________________________
library(ggplot2)
library(tidyr)
library(dipply)

df2 <- sum_all_dendro

df2$Yield.dry.beans..grams..per.kg.of.fruit.per.10.trees <- NULL
df2$Total.weight..kg..per.10.Trees <- NULL

head(df2)

#' Dodge side by side: X  = dendro, y = (value or abortion / and value healthy fruits) y1= Yield.beans.dry.per.hectare..kg.
df2%>%
  gather("Variable", "Value",- Dendro) %>%
  ggplot(aes(Dendro, Value, fill = Variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim = c(0, 2000))+
  scale_y_continuous(name = "Abortion/HealthyFruits",
                     sec.axis = sec_axis(~., name = "Yield.beans.dry.per.hectare..kg.")) +
  theme_bw()


library(ggplot2)
library(data.table)
length(df2$Dendro)
summary(df2$Dendro)

df2(df2[df2([1,1]:[25,1])])

df3 <- as.data.frame(df2[1:25,1])
wdf2 <- df2


df2$Abortion <- NULL

sec <- with(df2, train_sec(c(0, max(unemploy)),
                           c(0, max(psavert))))


df2%>% gather("Variable", "Value",- Dendro) %>%
  ggplot(aes(Dendro, Value, fill = Variable)) +
  geom_col(aes(y= Value), fill="blue") +
  geom_col(aes(y=Value * scaleFactor), fill="red") +
  scale_y_continuous(name="Healthy Fruits", sec.axis= sec_axis(~df2$rev(.), name="Yield beans dry per hectare kg")) +
  scale_x_discrete( as.numeric(c(df2[1:25,1]))) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  ) +
  labs(title = "Production and losses", x = element_blank())


#Other possibility______________________________________________________________________________________________________________________________

df2.m <- melt(as.data.table(df2, id.vars = 1))

ggplot(df2.m, aes(as.numeric(x= c(df2.m[1:25,1])), value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Yield.beans.dry.per.hectare..kg." = "blue", "HealthyFruits" = "red")) +
  facet_wrap(~variable, ncol = 1, scales = "free_y")+ 
  coord_cartesian(ylim = c(0, 2000))+
  scale_y_continuous(name = "Abortion/HealthyFruits",
                     sec.axis = sec_axis(~., name = "Yield.beans.dry.per.hectare..kg.")) +
  theme_bw()



str(df2)
df2$Abortion <- as.numeric(df2$Abortion)
df2$HealthyFruits <- as.numeric(df2$HealthyFruits)
df2$Dendro <- as.factor(df2$Dendro)


scaleFactor <- max(df2$HealthyFruits) / max(df2$Yield.beans.dry.per.hectare..kg.)
df2.m <- melt(df2, id.vars = 1)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#df_Flushes <- df %>% 
#group_by(date.x) %>%                         
#summarise_at(vars(Flushes),              
#list (mean(as.numeric(levels(Flushes[Flushes]))))     
#df$Flushes []
#levels(df$Flushes)
#mean_Flushes <- df %>%
#mutate(date = floor_date(df$date.x)) %>%
#group_by(date, Flushes) %>%
#summarize(mean_date= mean(date.x), by = list(df$date.x))
#summarize(mean(as.factor(levels(Flushes[Flushes]))))
