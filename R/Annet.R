
setwd("~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Annett")
df <- read.table("Auswertung_Spritzgenauigkeit.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE,  encoding = "latin1")
head(df)
str(df)

test_1 <- t.test(df$x1, df$y1)
summary(df$x1)
summary(df$y1)


op <- par(mfrow = c(1, 1))
a <- plot(x=df$x1, y=df$y1,
xlab= "ARA ausserhalb",
ylab= "Hand ausserhalb",
main= "ARA ausserhalb vs Hand ausserhalb")
lm1<- (lm(df$x1 ~ df$y1))
plot(lm1)
abline(lm(df$x1 ~ df$y1))
hist(x=df$x1, y=df$y1)


test_2 <- t.test(df$x2, df$y2)
b <- plot(x= df$x2, y=df$y2,
xlab= "ARA ungespritzt",
ylab= "Hand ungespritzt", 
main= "ARA ungespritzt vs Hand ungespritzt")
lm(df$x2 ~ df$y2)
abline(lm(df$x2 ~ df$y2))
hist(b)

