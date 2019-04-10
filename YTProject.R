#youtube Analytics Data:
#try to perform data science accompanied by great visuals

#Set WD:
setwd('C:\\Users\\Ralph\\Desktop\\R\\Projects')

#Upload and clean data:
original_data <- read.csv('data.csv', na.strings = "")

str(original_data)
wdata <- original_data

levels(wdata$Video.Uploads)
levels(wdata$Subscribers)

wdata <- wdata[wdata$Video.Uploads != "--",]
wdata <- wdata[wdata$Subscribers != "-- ",]

wdata$Video.Uploads <- as.numeric(as.character(wdata$Video.Uploads))
wdata$Subscribers <- as.numeric(as.character(wdata$Subscribers))

wdata <- wdata[complete.cases(wdata),]

#libraries:
library(tidyverse)

for(i in 1:length(wdata$Rank)){
  wdata$index[i] = i
}

wdata <- wdata[,c(7,2,3,4,5,6)]

#MINING:

#Video upload vs view count, and how it affects grade: 
ggplot(data = wdata) +
  geom_point(aes(x = Video.Uploads, y = Video.views, color = Grade)) +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')

#Video views influence grade more than the number of uploaded videos. without good views,
#A high video count can only bring you from B+ to an A-.

#Subscribers vs video views (we should see a positive slope trend).
ggplot(data = wdata) +
  geom_point(aes(x = Subscribers, y = Video.views, color = Grade))+
  geom_smooth(aes(x = Subscribers, y = Video.views), color = "Black") +
  facet_grid(.~Grade)
  

#as expected, we do see a positive slope. A++ grade has a higher ration of video views per subscriber rate:
appViewSubRatio <- mean(wdata[wdata$Grade == "A++ ", "Video.views"])/mean(wdata[wdata$Grade == "A++ ", "Subscribers"])
nappViewSubRatio <- mean(wdata[wdata$Grade != "A++ ", "Video.views"])/mean(wdata[wdata$Grade != "A++ ", "Subscribers"])

appViewSubRatio/nappViewSubRatio 
#a++/non a++ ratio is 2.241049. on average, a++ grade videos net more than 2x video views per video compared to non a++ grade videos.  
# this is further supported by the following violin plot:

ggplot(data = wdata) + 
  geom_boxplot(aes(x = Grade, y = Video.views, fill = Grade)) +
  geom_violin(aes(x = Grade, y = Video.views, color = Grade, alpha = 0.3)) +
  scale_y_continuous(trans = 'log10')

#let's remove the two outliers, so we can see a better look at the graph:
ggplot(data = wdata[wdata$Subscribers <= 39409726,]) +
  geom_point(aes(x = Subscribers, y = Video.views, color = Grade))+
  geom_smooth(aes(x = Subscribers, y = Video.views), color = "Black") 

#accounts per grade:
ggplot(data = wdata) +
  geom_bar(aes(x = Grade, fill = Grade))
#B+ to A- and A to A+ is a large jump, however, going from A- to A, and A+ to A++ isn't that big of a jump.
#this is supported by the first graph

#Let's create calculated fields, and look at their relationship with grade:
wdata$Average.views.per.video <- wdata$Video.views/wdata$Video.Uploads

ggplot(data = wdata) + 
  geom_boxplot(aes(x = Grade, y = Average.views.per.video, fill = Grade)) +
  scale_y_continuous(trans = 'log10') 

#interestingly enough, the A+ grade has a higher number of accounts with a higher Average View per video.
#in fact, the mean views per video is almost comparable to the lowest grade. this tells me that the outliers on
#the A++ grade really skew the values. this implies that some A++ videos receive a large view count that could be attributed
#to virality

#there is also a lot of outliers from the lower grade groups. let's see how video quality (views/subs) looks:

wdata$Average.subs.per.video <- wdata$Subscribers/wdata$Video.Uploads

ggplot(data = wdata) + 
  geom_boxplot(aes(x = Grade, y = Average.subs.per.video, fill = Grade)) +
  scale_y_continuous(trans = 'log10')

# the A++ group actually have the lower number of subs, indicative of virality

#In conclusion, video views are what determine grades the most. viral videos are what spearate the A+ group from the A++ group.



