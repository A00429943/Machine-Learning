library("readr")
library("ggplot2")
library("ggthemes")
library("dplyr")
library("stats")
library("corrgram")
library("corrplot")
library("caTools")

install.packages('caTools')


df <- read.csv("C:/Users/maninderk770/Desktop/R/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/student-mat.csv",";")
head(df)
summary(df)

any(is.na(df))
str(df)

#corelation plots (dependencies)
#Num only

num.cols <- sapply(df,is.numeric)

#filter

cor.data<- cor(df[,num.cols])

print(cor.data)

#corplot only grabs numeric column
print(corrplot(cor.data,method = 'color'))

#


corrgram(df,order=TRUE, lower.panel=panel.shade, upper.panel = panel.pie, text.panel = panel.txt)
print(ggplot(df, aes(x=G3)) + geom_histogram(bins = 20,alpha = 0.5, fill = 'blue'))

#splitting the data training set and testing set
#set A Seed

set.seed(101)

#split up sample
sample <- sample.split(df$G3,SplitRatio = 0.7)

#70% of data -> train
train <- subset(df,sample == TRUE)

#30% will be test
test <- subset(df,sample == FALSE)

#train and build model

model <- lm(G3 ~ ., data = train)

#run model

#intrepret the model
print(summary(model))

res<- residuals(model)
class(res)
res <- as.data.frame(res)
head(res)
print(ggplot(res,aes(res)) + geom_histogram(fill='blue', alpha= 0.5))

#advanced data visualization
plot(model)

#predictions

G3.predictions<-predict(model,test)


#
results <-cbind(G3.predictions,test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
head(results)

#take care of negative values
to_zero <-function(x){
  if(x<0){
    return(0)
  }else{
    return(x)
    }
}

#Apply Zero Function
results$predicted <- sapply(results$predicted, to_zero)

#Mean Squared error
mse <- mean((results$actual - results$predicted)^2)
print(mse)

#RMSE
print(mse^0.5)

####

SSE <- sum((results$predicted - results$actual)^2)

SST <- sum ((mean(df$G3) - results$actual)^2)
R2 <- 1- SSE/SST
print('R2')
print(R2)
           
