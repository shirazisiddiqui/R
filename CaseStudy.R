DATA_1 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 1.txt",header=T)
DATA_2 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 2.txt",header=T)
DATA_3 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 3.txt",header=T)
DATA_4 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 4.txt",header=T)

DATA_5 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 5.txt",header=T)
DATA_6 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 6.txt",header=T)
DATA_7 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 7.txt",header=T)
DATA_8 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 8.txt",header=T)

DATA_9 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 9.txt",header=T)
DATA_10 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 10.txt",header=T)
DATA_11 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 11.txt",header=T)
DATA_12 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 12.txt",header=T)

DATA_13 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 13.txt",header=T)
DATA_14 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 14.txt",header=T)
DATA_15 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 15.txt",header=T)
DATA_16 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 16.txt",header=T)

DATA_17 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 17.txt",header=T)
DATA_18 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 18.txt",header=T)
DATA_19 <- read.table("/Users/shirazisiddiqui/Documents/DSS665/Vegetation Data 19.txt",header=T)

veg_data <- rbind(DATA_1,DATA_2,DATA_3,DATA_4,DATA_5,DATA_6,DATA_7,DATA_8,DATA_9,DATA_10,DATA_11,DATA_12,DATA_13,DATA_14,DATA_15,DATA_16,DATA_17,DATA_18,DATA_19)

dim(veg_data)

head(veg_data)

site <- veg_data[,1]
xcoord <- veg_data[,2]
ycoord <- veg_data[,3]
head(site)
head(xcoord)
head(ycoord)

###Plots,600 observations
time.code <- 1:600

####To get site 
veg_data[7000,1]
veg_data[16000,1]
veg_data[48000,1]

plot(time.code,veg_data[1,4:603],"l") ###These columns contain NDVI values

plot(time.code,veg_data[1000,4:603],"l") 

par(mfrow=c(3,1))
plot(time.code,veg_data[7000,4:603],"l",main ="N16090") 
plot(time.code,veg_data[16000,4:603],"l",main ="N26614") 
plot(time.code,veg_data[48000,4:603],"l",main ="N07288") 

#############################################
###Average NDVI values

veg_data[1,4:603]

as.numeric(veg_data[1,4:603])
as.numeric(veg_data[2,4:603])

avg.NDVI <- c(rep(0,49681))
avg.NDVI
for (i in 1:49681){
  avg.NDVI[i] <- mean(as.numeric(veg_data[i,4:603]))
}

head(avg.NDVI,100)

veg.avgs <- cbind(xcoord,ycoord,avg.NDVI)
head(veg.avgs,20)

plot(xcoord,ycoord,pch=20,col='gray' , main ='Map of Avg NDVI at each site',xlab="Latitude",ylab="longitude")

for(i in 1:49681) {
  if(avg.NDVI[i] < 200) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="red")
  if(avg.NDVI[i] > 200 & avg.NDVI[i] < 400) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="tan")
  if(avg.NDVI[i] > 400 & avg.NDVI[i] < 600) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="lightgreen")
  if(avg.NDVI[i] > 600 & avg.NDVI[i] < 700) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="green")
  if(avg.NDVI[i] > 700) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="darkgreen")
}

legend(x=40,y=-4.0,c("NDVI<200","200<NDVI<400","400<NDVI<600","600<NDVI<700","NDVI>700"),fill=c("red","tan","lightgreen","green","darkgreen"))

###########################################################
#Which area has increasing and decreasing vegetation trend--calculate the yearly averages for site 1
veg_data[1,]
veg_data[1,4:603]

yearly_avg <- matrix(c(rep(0,49681 * 25)),nrow=49681,ncol=25)
for(i in (1:49681))
{
yearly_avg[i,1] <- mean(as.numeric(veg_data[i,4:27])) ###NDVI average for 1982 (site#1)
yearly_avg[i,2] <- mean(as.numeric(veg_data[i,28:51])) ###NDVI average for 1983 (site#1)
yearly_avg[i,3] <- mean(as.numeric(veg_data[i,52:75])) ###NDVI average for 1984 (site#1)
yearly_avg[i,4] <- mean(as.numeric(veg_data[i,76:99])) ###NDVI average for 1985 (site#1)
yearly_avg[i,5] <- mean(as.numeric(veg_data[i,100:123])) ###NDVI average for 1986 (site#1)
yearly_avg[i,6] <- mean(as.numeric(veg_data[i,124:147])) ###NDVI average for 1987 (site#1)
yearly_avg[i,7] <- mean(as.numeric(veg_data[i,148:171])) ###NDVI average for 1988 (site#1)
yearly_avg[i,8] <- mean(as.numeric(veg_data[i,172:195])) ###NDVI average for 1989 (site#1)
yearly_avg[i,9] <- mean(as.numeric(veg_data[i,196:219])) ###NDVI average for 1990 (site#1)
yearly_avg[i,10] <- mean(as.numeric(veg_data[i,220:243])) ###NDVI average for 1991 (site#1)
yearly_avg[i,11] <- mean(as.numeric(veg_data[i,244:267])) ###NDVI average for 1992 (site#1)
yearly_avg[i,12] <- mean(as.numeric(veg_data[i,268:291])) ###NDVI average for 1993 (site#1)
yearly_avg[i,13] <- mean(as.numeric(veg_data[i,292:315])) ###NDVI average for 1994 (site#1)
yearly_avg[i,14] <- mean(as.numeric(veg_data[i,316:339])) ###NDVI average for 1995 (site#1)
yearly_avg[i,15] <- mean(as.numeric(veg_data[i,340:363])) ###NDVI average for 1996 (site#1)
yearly_avg[i,16] <- mean(as.numeric(veg_data[i,364:387])) ###NDVI average for 1997 (site#1)
yearly_avg[i,17] <- mean(as.numeric(veg_data[i,388:411])) ###NDVI average for 1998 (site#1)
yearly_avg[i,18] <- mean(as.numeric(veg_data[i,412:435])) ###NDVI average for 1999 (site#1)
yearly_avg[i,19] <- mean(as.numeric(veg_data[i,436:459])) ###NDVI average for 2000 (site#1)
yearly_avg[i,20] <- mean(as.numeric(veg_data[i,460:483])) ###NDVI average for 2001 (site#1)
yearly_avg[i,21] <- mean(as.numeric(veg_data[i,484:507])) ###NDVI average for 2002 (site#1)
yearly_avg[i,22] <- mean(as.numeric(veg_data[i,508:531])) ###NDVI average for 2003 (site#1)
yearly_avg[i,23] <- mean(as.numeric(veg_data[i,532:555])) ###NDVI average for 2004 (site#1)
yearly_avg[i,24] <- mean(as.numeric(veg_data[i,556:579])) ###NDVI average for 2005 (site#1)
yearly_avg[i,25] <- mean(as.numeric(veg_data[i,580:603])) ###NDVI average for 2006 (site#1)  
}
head(yearly_avg)
dim(yearly_avg)

#### Pick up few sites to plot yearly averages
###Site#500
years <- 1:25
plot(years,yearly_avg[500,],"l",xlab="1982-2006",ylab="NDVI SCORE",main="Averaged NDVI values")
abline(lm(yearly_avg[500,] ~ years),col="red")
summary(lm(yearly_avg[500,] ~ years)) ### No significant changes as P-value is 0.7153
###Site#226
years <- 1:25
plot(years,yearly_avg[226,],"l",xlab="1982-2006",ylab="NDVI SCORE",main="Averaged NDVI values")
abline(lm(yearly_avg[226,] ~ years),col="red")
summary(lm(yearly_avg[226,] ~ years)) ### Significant increasing trends as p-value is 0.00098
###Site#30881
years <- 1:25
plot(years,yearly_avg[30881,],"l",xlab="1982-2006",ylab="NDVI SCORE",main="Averaged NDVI values")
abline(lm(yearly_avg[30881,] ~ years),col="red")
summary(lm(yearly_avg[30881,] ~ years)) ### Significant decreasing trend as p-value is 0.02

###Extract p-value and slope information
summary(lm(yearly_avg[30881,] ~ years))$coefficients
summary(lm(yearly_avg[30881,] ~ years))$coefficients[2,4] ### Extract only p-value
summary(lm(yearly_avg[30881,] ~ years))$coefficients[2,1] ### Extract only slope
sign(summary(lm(yearly_avg[30881,] ~ years))$coefficients[2,1]) ### Extract sign of slope

### create a loop to grab only all p-values and slopes info for all sites

pvals <- c(rep(0,49681))
slopes <- c(rep(0,49681))

for(i in 1:49681)
{
pvals[i] <- summary(lm(yearly_avg[i,] ~ years))$coefficients[2,4] ### Extract only p-value
slopes[i] <- sign(summary(lm(yearly_avg[i,] ~ years))$coefficients[2,1]) ### Extract only slope
}

head(pvals,30)
head(slopes,30)

###Information on Map###
#sites and color code as follows###
#if p-value > 0.01, plot in tan,indicating no extreme change in vegetation
#if p < 0.01 and slope is +ve, plot in green indicating increasing change in vegetation
#if p < 0.01 and slope is -ve, plot in red indicating decreasing change in vegetation

plot(xcoord,ycoord,pch=20,col='gray' , main ='Map of Extreme changing NDVI',xlab="Latitude",ylab="longitude")

for(i in 1:49681) {
  if(pvals[i] > 0.01) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="tan")
  if(pvals[i] < 0.01 & slopes[i]==1 ) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="green")
  if(pvals[i] < 0.01 & slopes[i]==-1 ) points(xcoord[i],ycoord[i],pch=20,cex=0.6,col="red")
}

legend(x=40,y=-4.0,c("NO NDVI CHANGES","increasing change in trend","Decreasing change in trend"),fill=c("tan","green","red"))

#####Extreme increasing NDVI score with pvalue<0.1 = count.inc
#Extreme increasing NDVI score with pvalue<0.1 = count.dec
#Extreme increasing NDVI score with pvalue<0.1 = count.insig

count.inc=0
count.dec=0
count.insig = 0
for (i in 1:49681){
  if(pvals[i] < 0.01 & slopes[i]==1) count.inc= count.inc + 1
  if(pvals[i] < 0.01 & slopes[i]==-1) count.dec= count.dec + 1
  if(pvals[i] > 0.01) count.insig= count.insig+1
}
count.inc    
count.dec     
count.insig  
#View extreme increasing and extreme decreasing trend site information
site.info <- cbind(site, pvals, slopes, yearly_avg)
dim(site.info)
head(site.info)


#increasing (N12250)
for (i in 1:49681){
  if(pvals[i]<0.0001 & slopes[i]==1) 
    print(site.info[i])
}
#decreasing (N13805)
for (i in 1:49681){
  if(pvals[i]<0.0001 & slopes[i]==-1) 
    print(site.info[i])
}

yearly_avg[3, ]
#Plotting trends
year <- 1:25
par(mfrow = c(2, 1))
#increasing trend plot
plot(years, yearly_avg[3, ], "l", ylim = c(200,700), xlab= "1982-2006", ylab= "NDVI Score", main="Extreme Inc. Site N09003")
abline(lm(yearly_avg[3, ] ~ years), col="red")

#decreasing trend plot
plot(years, yearly_avg[1993, ], "l", ylim = c(200,700), xlab= "1982-2006", ylab= "NDVI Score", main="Extreme Dec. Site N11014")
abline(lm(yearly_avg[1993, ] ~ years), col="red")

#map
site.location<- data.frame(site, yearly_avg, xcoord, ycoord)
site.location

points(41.05497, 2.755276, pch= 4, cex =0.9, col="darkgreen")
points(30.50952, 1.955276, pch= 4, cex =0.9, col="darkred")