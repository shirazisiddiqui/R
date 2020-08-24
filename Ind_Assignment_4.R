###Assignment4 ind
pvals <- c(rep(0,49681))
slopes <- c(rep(0,49681))

for(i in 1:49681)
{
  pvals[i] <- summary(lm(yearly_avg[i,] ~ years))$coefficients[2,4] ### Extract only p-value
  slopes[i] <- sign(summary(lm(yearly_avg[i,] ~ years))$coefficients[2,1]) ### Extract only slope
}

head(pvals,30)
head(slopes,30)

###Increasing site
which(pvals<0.001 & slopes==1)

###Site Name
veg_data[48826,1] ##N08125

years_24 <- 24 * years

plot(time.code, veg_data[48826,4:603 ],"l", ylim = c(100,600),
     main = paste("Combo Plot for Inc. change in Veg of site:",veg_data[48826,1]),sub=paste("p-value =",round(pvals[48826], 4)," ,Slope =", round(slopes[48826], 4)),
     ylab = "NDVI Score", xlab = "1982-2006")
lines(years_24, yearly_avg[48826, ], col = "blue",type='b',pch=20)
abline(lm(yearly_avg[48826, ]~ years_24), col = "red")
legend(x=500,y=200,c("NDVI Data","Yearly average","Trend"),fill=c("black","blue","red"),text.width = 0.5,cex=0.7,bty='n')


###Decreasing site
which(pvals<0.001 & slopes==-1)

###Site Name
veg_data[36512,1] ##N52030

years_24 <- 24 * years

plot(time.code, veg_data[36512,4:603 ],"l", ylim = c(100,600),
     main = paste("Combo Plot for Dec. change in Veg of site:",veg_data[36512 ,1]),sub=paste("p-value =",round(pvals[36512], 4)," ,Slope =", round(slopes[36512], 4)),
     ylab = "NDVI Score", xlab = "1982-2006")
lines(years_24, yearly_avg[36512, ], col = "blue",type='b',pch=20)
abline(lm(yearly_avg[36512, ]~ years_24), col = "red")
legend(x=450,y=500,c("NDVI Data","Yearly average","Trend"),fill=c("black","blue","red"),text.width = 0.5,cex=0.7,bty='n')


