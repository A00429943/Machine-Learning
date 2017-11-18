
bike <- read.csv("C:/Users/maninderk770/Desktop/R/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bikeshare.csv")
head(bike)

## EDA

library("ggplot2")
library("dplyr")
ggplot(bike,aes(temp,count)) + geom_point(alpha = 0.3, aes(color=temp)) + theme_bw()

# Comvert POSIXct()

bike$datetime <- as.POSIXct(bike$datetime)

pl<- ggplot(bike,aes(datetime,count)) + geom_point(alpha = 0.3, aes(color=temp)) + theme_bw()
pl+scale_color_continuous(low='#55D8CE', high = '#FF6E2E') + theme_bw()

# Feature Engineering

bike$hour <- sapply(bike$datetime, function(x){format(x, "%H")})
head(bike)
bike$hour<- sapply(bike$hour,as.numeric)


# Scatterplot  1 -> working day 0 non working day
pl <- ggplot(filter(bike,workingday==1),aes(hour,count))
####
pl<- pl+geom_point(aes(color=temp), position = position_jitter(w=1,h=0), alpha=0.5)
pl<- pl+ scale_color_gradientn(colors=c('dark blue', 'blue', 'light blue','yellow','red','orange','purple','pink'))
print(pl+theme_bw())

## buid model
# model<-lm(count~column1 + column2 +...,bike)
# model <- lm(count~. -casual -registered ..., bike )

#temp.model <- lm(count~ temp,bike)

model <- lm(count~. -casual -registered -datetime -atemp, bike )


##
#print(summary(temp.model))

print(summary(model))

#how many bike rental counts at 25c?
6.0462 + 9.17*25

temp.test <- data.frame(temp=c(25))
temp.test
predict(temp.model,temp.test)


