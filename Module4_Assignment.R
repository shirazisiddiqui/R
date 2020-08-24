###Question1

?chickwts
chickwts
attach(chickwts)

boxplot(feed~weight)
chicken <- aov(weight ~ feed, data=chickwts)
summary(chicken)
boxplot(chickwts$weight~chickwts$feed)
TukeyHSD(chicken,"feed")
boxplot(feed~weight)

###Difference between horsebean-casein is highly significant as per p-value and Boxplot.P-value=0.00000
###Difference between sunflower-horsebean is highly significant as per p-value and Boxplot.P-value=0.00000

###Difference between sunflower-linseed is quite significant as per p-value and Boxplot.P-value=0.00008
###Difference between meatmeal-horsebean is quite significant as per p-value and Boxplot.P-value=0.0001
###Difference between linseed-casein is quite significant as per p-value and Boxplot.P-value=0.0002

###Difference between sunflower-soybean is significant as per p-value and Boxplot.P-value=0.003
###Difference between soybean-horsebean is significant as per p-value and Boxplot.P-value=0.004
###Difference between soybean-casein is significant as per p-value and Boxplot.P-value=0.008

###Question2
#a
retail_sales <- read.csv("/Users/shirazisiddiqui/Documents/DSS665/Retail_Sales.csv",header=T)
attach(retail_sales)
#b
retail_sales

#c create dummy variable

Quarter.Q1 <- c(rep(0,40))
Quarter.Q1
for(i in 1:40){
  if(Quarter[i]=="Q1")
    Quarter.Q1[i]=1
}
Quarter.Q1

Quarter.Q2 <- c(rep(0,40))
Quarter.Q2
for(i in 1:40){
  if(Quarter[i]=="Q2")
    Quarter.Q2[i]=1
}
Quarter.Q2

Quarter.Q3 <- c(rep(0,40))
Quarter.Q3
for(i in 1:40){
  if(Quarter[i]=="Q3")
    Quarter.Q3[i]=1
}
Quarter.Q3

retail_sales.recoded <- cbind(Year,Quarter.Q1,Quarter.Q2,Quarter.Q3, Retail.sales..in.millions.,GNP..in.billions.)
retail_sales.recoded

#d.
##Linear Model: First run
summary(lm(Retail.sales..in.millions. ~ GNP..in.billions.+ Quarter.Q1 + Quarter.Q2 + Quarter.Q3))

##Anona of Linear Model:First run
anova(lm(Retail.sales..in.millions. ~ GNP..in.billions.+ Quarter.Q1 + Quarter.Q2 + Quarter.Q3))

###Since Quarter.Q2 p-value is not significant,so drop it and rerun:
##Linear Model : Second run
summary(lm(Retail.sales..in.millions. ~ GNP..in.billions.+ Quarter.Q1 + Quarter.Q3))
##Anona of Linear Model:Second run
anova(lm(Retail.sales..in.millions. ~ GNP..in.billions.+ Quarter.Q1 + Quarter.Q3))

##f
resid.MLR = resid(lm(Retail.sales..in.millions. ~ GNP..in.billions.+ Quarter.Q1 + Quarter.Q3))
hist(resid.MLR)
plot(resid.MLR)
shapiro.test(resid.MLR) ###Should we worry about the assumption of MLR? Yes as p-value is very low

###Question3

MPG <- read.csv("/Users/shirazisiddiqui/Documents/DSS665/MPG.csv",header=T)
attach(MPG)
MPG

mpg1 = factor(ifelse(MPG$mpg > median(MPG$mpg,na.rm=T), 1, 0))
mpg1

CARMPG <- cbind(MPG,mpg1)
CARMPG

tail(CARMPG)
       
origin1 = factor(MPG$origin,levels = c(1,2,3),labels = c('American', 'German', 'Japanese'))
levels(origin1)

# Create visualizations of the relationship between mpg_binary and 
# other variables.
table(origin1)
barplot(table(origin1), col=2:4, main = "Barplot of Origin", xlab = "Origin", ylab = "frequency")

bp <- data.frame(CARMPG$mpg1,origin1)
table(bp)
barplot(table(bp),beside=T, col=4:5, legend=T, main = "Barplot of Origin by MPG binary", xlab = "Origin", ylab = "frequency")

barplot(table(cylinders))
bp2 <- data.frame(mpg1,cylinders)
barplot(table(bp2),beside=T, col=4:5, legend=T,main = "Barplot of Cylinders", xlab = "Cylinders", ylab = "frequency")

by(MPG[,3],mpg1,summary)
by(MPG[,4],mpg1,summary)
by(MPG[,5],mpg1,summary)
by(MPG[,6],mpg1,summary)

#Logistic Regression Model
Logistic.model <- glm(mpg1 ~ as.factor(cylinders) + displacement + 
                        Horsepower + CARMPG$weight + acceleration + as.factor(CARMPG$year) + 
                        as.factor(origin1), family= "binomial")

summary(Logistic.model)

#Logistic Model without the year variable
Logistic.model1 <- glm(mpg1 ~ as.factor(cylinders) + displacement + 
                         Horsepower + CARMPG$weight + acceleration +  
                         as.factor(origin1), family= "binomial")
summary(Logistic.model1)

#Logistic Model without the year and weight variable
Logistic.model2 <- glm(mpg1 ~ as.factor(cylinders) + displacement + 
                         Horsepower + acceleration +  
                         as.factor(origin1), family= "binomial")
summary(Logistic.model2)

#Predict the probability
pred_car <- data.frame(cylinders = (6), 
                       displacement = (297), 
                       Horsepower = (155), 
                       weight = (3530),
                       acceleration =(13.4),
                       year = (79),
                       origin1 = ("American"))
pred_car

predict(Logistic.model2, pred_car)

