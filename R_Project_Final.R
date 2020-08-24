SPTFY_DATA_BEFORE_PANDEMIC <- read.csv("/Users/shirazisiddiqui/Documents/DSS665/FinalMasterList2020_beforePandemic.csv",header=T)
SPTFY_DATA_AFTER_PANDEMIC <- read.csv("/Users/shirazisiddiqui/Documents/DSS665/FinalMasterList2020_afterPandemic.csv",header=T)

attach(SPTFY_DATA_BEFORE_PANDEMIC)
attach(SPTFY_DATA_AFTER_PANDEMIC)

head(SPTFY_DATA_BEFORE_PANDEMIC)
head(SPTFY_DATA_AFTER_PANDEMIC)

tail(SPTFY_DATA_BEFORE_PANDEMIC)
tail(SPTFY_DATA_AFTER_PANDEMIC)

summary(SPTFY_DATA_BEFORE_PANDEMIC)
summary(SPTFY_DATA_AFTER_PANDEMIC)

max(SPTFY_DATA_BEFORE_PANDEMIC$Streams) ### Maximum Stream before Pandemic
min(SPTFY_DATA_BEFORE_PANDEMIC$Streams) ### Minimum Stream before Pandemic 

max(SPTFY_DATA_AFTER_PANDEMIC$Streams) ### Maximum Stream after Pandemic
min(SPTFY_DATA_AFTER_PANDEMIC$Streams) ### Minimum Stream after Pandemic

SPTFY_DATA_BEFORE_PANDEMIC[1:5,3:5] ### Most played Tracks Before Pandemic
SPTFY_DATA_AFTER_PANDEMIC[1:5,3:5] ### Most played Tracks After Pandemic

TOP_5_SONGS_BP=head(SPTFY_DATA_BEFORE_PANDEMIC,5)
TOP_5_SONGS_AP=head(SPTFY_DATA_AFTER_PANDEMIC,5)

### Barplots for Top 5 most played tracks
par(mfrow=c(2,1))
barplot(TOP_5_SONGS_BP$Streams ~ TOP_5_SONGS_BP$Track.Name,col=2:7,legend=T, main = "Barplot of Top 5 Songs before Pandemic", xlab = "Track Name", ylab = "Streams")
barplot(TOP_5_SONGS_AP$Streams ~ TOP_5_SONGS_AP$Track.Name,col=2:7,legend=T, main = "Barplot of Top 5 Songs after Pandemic", xlab = "Track Name", ylab = "Streams")

####Dataframe for numeric data
SPTFY_DATA_BP_numeric <- as.data.frame(cbind(danceability=SPTFY_DATA_BEFORE_PANDEMIC$danceability,energy=SPTFY_DATA_BEFORE_PANDEMIC$energy,key=SPTFY_DATA_BEFORE_PANDEMIC$key,loudness=SPTFY_DATA_BEFORE_PANDEMIC$loudness,mode=SPTFY_DATA_BEFORE_PANDEMIC$mode,speechiness=SPTFY_DATA_BEFORE_PANDEMIC$speechiness,accousticness=SPTFY_DATA_BEFORE_PANDEMIC$acousticness,instrumentalness = SPTFY_DATA_BEFORE_PANDEMIC$instrumentalness,liveness=SPTFY_DATA_BEFORE_PANDEMIC$liveness,valence=SPTFY_DATA_BEFORE_PANDEMIC$valence,tempo=SPTFY_DATA_BEFORE_PANDEMIC$tempo,duration_ms=SPTFY_DATA_BEFORE_PANDEMIC$duration_ms))
head(SPTFY_DATA_BP_numeric)

SPTFY_DATA_AP_numeric <- as.data.frame(cbind(danceability=SPTFY_DATA_AFTER_PANDEMIC$danceability,energy=SPTFY_DATA_AFTER_PANDEMIC$energy,key=SPTFY_DATA_AFTER_PANDEMIC$key,loudness=SPTFY_DATA_AFTER_PANDEMIC$loudness,mode=SPTFY_DATA_AFTER_PANDEMIC$mode,speechiness=SPTFY_DATA_AFTER_PANDEMIC$speechiness,accousticness=SPTFY_DATA_AFTER_PANDEMIC$acousticness,instrumentalness = SPTFY_DATA_AFTER_PANDEMIC$instrumentalness,liveness=SPTFY_DATA_AFTER_PANDEMIC$liveness,valence=SPTFY_DATA_AFTER_PANDEMIC$valence,tempo=SPTFY_DATA_AFTER_PANDEMIC$tempo,duration_ms=SPTFY_DATA_AFTER_PANDEMIC$duration_ms))
head(SPTFY_DATA_AP_numeric)

###Correlation Matrix
library(corrplot)

corrplot(cor(SPTFY_DATA_BP_numeric),
         method = "number",
         type = "upper" # show only upper side
)

corrplot(cor(SPTFY_DATA_AP_numeric),
         method = "number",
         type = "upper" # show only upper side
)

########
library(ggplot2)

ggplot(data = SPTFY_DATA_BEFORE_PANDEMIC, aes(x = valence, y = energy , col= week_of)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry", fontface ="bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyful", fontface ="bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Peaceful", fontface ="bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressing", fontface ="bold")

#####
ggplot(data = SPTFY_DATA_AFTER_PANDEMIC, aes(x = valence, y = energy , col=week_of)) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry", fontface ="bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Joyful", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Peaceful", fontface ="bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Depressing", fontface ="bold")








