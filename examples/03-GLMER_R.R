#Read all the files saved in 06-Paper_1_R____________________________________________________________________________________________________________________________________________________________________
# Read Data from Path ~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/262.2_VT_Nutztierhaltung/CowData/02_Work/Analyse_MVS_[comr]/03_Data_In/
require(lubridate)
require(dplyr)

files <- dir(path = "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R", pattern = "[.]csv$", full.names = TRUE)
files

df <- files %>%
  rio::import_list(setclass = class(tibble()), rbind = TRUE, header=TRUE, sep=";", stringsAsFactors = FALSE)

head(df)
str(df)

df$date <- as.Date(df$date, format = "%d.%m.%Y")


df$dateandtime <- as.POSIXct(strptime(paste(df$date, 
                                            
                                            format(df$hour, format = "%H:%M")),
                                      
                                      format="%Y-%m-%d %H:%M"))        



#### Statistik Stem expansion

require("lme4")
require("MuMIn")
library("lme4")
require("lme4")
library("MuMIn")
require("MuMIn")
require("dplyr")
require("tidyr")
require("ggplot2")

setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/06-Paper_1_R/GLMER_files")

df <- read.table("df4.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE,  encoding = "latin1")
head(df)
str(df)
df$Date <-  as.Date(df$Date , format = "%d.%m.%Y")
table(complete.cases(df))
df3 <- df[complete.cases(df),]
df$Flushes.x == 2


dim (df3)
names (df3)
str(df3)
summary(df3)

df <- df3
str(df$Date)
#Define factors
df$Date                        <- as.Date     (df$Date, format ="%Y-%m-%d")
df$Flushes.x                   <- as.factor   (df$Flushes.x)
df$Median20cm                  <- as.numeric  (df$Median20cm)
df$Media40cm                   <- as.numeric  (df$Media40cm)
df$Flowers.x                   <- as.factor   (df$Flowers.x)
df$ETP                         <- as.numeric  (df$ETP)
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

summary(df)
str(df)
head(df)

#Define data subset

# 2 Step

par (mfrow= c (2, 2))
df$resid <- residuals(dendro.lmer, type="pearson")
scatter.smooth (fitted (dendro.lmer), resid (dendro.lmer))
qqnorm(resid(dendro.lmer))
boxplot (split (resid (dendro.lmer), df[, 'Flushes.x']))
hist(df$resid)


#logit(p, percents=max(p, na.rm = TRUE) > 1, adjust) 
#df.df$P_MVC<- df.df$P_MVC/100

df[, df$Flushes.x] == 2

# 1 step
boxcox(as.numeric(df$MDS))


dendro.lmer <- lmer((MDS) ~ (ETP + Flushes.x) +
                     (1|Abortion) +(1|Dendro),df,REML = FALSE, na.action = na.fail)


anova <-anova(dendro.lmer)
summary(anova)

rePCA <- rePCA(dendro.lmer)
?isSingular

#(log(mean_dendro/(1-mean_dendro))
#find bes model
dredge <- dredge(dendro.lmer, rank= 'BIC', trace= TRUE)
dredge
#look at best model
getCall (dredge, '4')

best_model<- lmer(formula = (MDS) ~ ETP + Flushes.x + (1 | Abortion) + (1 |  Dendro), 
                  data = df, REML = FALSE, na.action = na.fail)
>                                

dat <- expand.grid (ETP =unique (df [, 'ETP']) [order (unique (df [, 'ETP']))], 
                    Flushes.x= unique (df [, 'Flushes.x']) [order (unique (df [, 'Flushes.x']))])

fixef (best_model)


estim.Mod <- function (x) {
  predict (x, newdata= dat, re.form= NA) }

mod.boot <- bootMer (best_model, estim.Mod, nsim= 100, seed = 1, parallel="multicore", ncpus = 4)

extract.ci <- function (x) {
  out <- data.frame (numeric (0), numeric (0), numeric (0))
  for (i in 1:length (x [['t0']])) {
    out <- rbind (out, c (x [['t0']] [i],
                          boot.ci (x, index= i, type= 'perc') [['percent']] [, 4:5]))
  }
  names (out) <- c ('estim', 'lo.ci', 'up.ci')
  out
}

#inklusive Rücktransformation

mod.estim <- (extract.ci (mod.boot))
mod.estim <- cbind (dat, mod.estim)

library(gmodels)
par (mfrow= c (1,1))
boxplot (DG ~ Flushes.x, df)

title(main="Max Daily Shrinkage")
axes=FALSE
xlab=FALSE
title(ylab="MDS [μm]", )
title(xlab="Flushes [observation classes]", )


lines (c(1:16), mod.estim [(1:16), 'estim'], lwd= 2, col="grey40") 
lines (c(1:16), mod.estim [(1:16), 'lo.ci'], lwd= 2, lty= 2, col="grey40") 
lines (c(1:16), mod.estim [(1:16), 'up.ci'], lwd= 2, lty= 2,col="grey40")
