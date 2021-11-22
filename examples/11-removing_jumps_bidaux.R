#Here, we remove the jumps
library(dendRoAnalyst)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

idir <- "~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/raw_data"
dendro <- read.table(file.path(idir, "dendro_15min.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE)
odir <- '"~/mnt/Data-Work-RE/26_Agricultural_Engineering-RE/263_DP/03_Persoenliche_Unterlagen/Wata/08-R_dendro_files/Bidaux/dendro_nogaps15min'


#Case of  (TreeNum = 1)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 1, v = 100)

plot(s$T3B1, type = 'l')
r <- runmed(s$T3B1, k = 25) #median filter 
plot(r, type = 'l')
#Manually correct it

#r <- data.frame(
  #date = s$date,
  #T3B1 = r)


#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B1)) +
  #geom_line()
#ggplotly(p)


#r$T3B1[2098:2394] <- r$T3B1[2394:(2098+296)]

#Plot and see if you are satisfied from the results
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B1)) +
  #geom_line()
#ggplotly(p)


#Save them in the project folder 
write.table(r, file.path(odir, paste0(colnames(r)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 2)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 2, v =  100)

plot(s$T3B3, type = 'l')

write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 3)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 3, v =  100)

plot(s[,2], type = 'l')

#p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B4)) +
#geom_line()
#ggplotly(p)



write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of (TreeNum = 4)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 4, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
  #geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 5)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 5, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 6)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 6, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 7)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 7, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

colnames(dendro)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 8)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 8, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 9)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 9, v =  100)

#Plot and see if you are satisfied from the results
#p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  #geom_line()
#ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 10)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 10, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 11)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 11, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 12)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 12, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 13)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 13, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 14)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 14, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 15)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 15, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 16)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 16, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 17)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 17, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 18)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 18, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 19)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 19, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 20)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 20, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 21)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 21, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 22)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 22, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 23)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 23, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 24)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 24, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 25)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 25, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 26)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 26, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 27)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 27, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 28)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 28, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 29)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 29, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 30)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 30, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 31)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 31, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 32)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 32, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


#Case of  (TreeNum = 33)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 33, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 34)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 34, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")

#Case of  (TreeNum = 35)
###############################################################################
#Locate jumps
s <- jump.locator(dendro, TreeNum = 35, v =  100)

#Plot and see if you are satisfied from the results
p <- ggplot(data = s, aes(x = 1:nrow(s), y = T3B2)) +
  geom_line()
ggplotly(p)

#Manually correct it
#r <- s
#r$T3B2[1:1474] <- r$T3B2[1:1474] - (r$T3B2[1475] - r$T3B2[1454])

#Plot it again and see if you are satisfied
#p <- ggplot(data = r, aes(x = 1:nrow(s), y = T3B2)) +
#geom_line()
#ggplotly(p)

#Save them in the project folder 
write.table(s, file.path(odir, paste0(colnames(s)[2],'.csv')), sep = ";")


plot

