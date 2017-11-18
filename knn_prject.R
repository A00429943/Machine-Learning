library('ISLR')
print(head(iris))
#dataset that predicts species of iris flower

## Scale the data

stand.features <- scale(iris[1:4])
print(var(stand.features[,1]))

###
final.data <- cbind(stand.features,iris[5])
###
##Train test splits
###

set.seeds(101)
library(caTools)
sample<- sample.split(final.data$Species, SplitRatio = 0.7)

train <- subset(final.data, sample == T)
test <- subset(final.data, sample == F)

##
##knn
##

library('class')
predicted.species <- knn(train[1:4],test[1:4],train$Species, k=1)

print(mean(test$Species != predicted.species))

#####
#### Choose a  K value

predicted.species <- NULL
error.rate <- NULL

for (i in 1:10) {
  set.seed(101)
predicted.species <- knn(train[1:4], test[1:4],train$Species,k=i)
error.rate[i] <- mean(test$Species != predicted.species)
}

### plot this out for the elbow method

library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
ggplot(error.df,aes(k.values,error.rate))+geom_point()+geom_line(lty='dotted',color='red')
