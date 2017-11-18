# Classification problem using Logistic regression solving
# problems and interpretating results
#examples binary Classification(2 classes -> 0 and 1)
## spam versus "Ham" emails
## Loan default
## Disease Diagnosis 
#logistic regression curve
#sigmoid (logistic function) -> any value -> output 0 and 1
library("readr")
library("Amelia")
library("ggplot2")
library("dplyr")
library("caTools")

df.train <- read.csv("C:/Users/maninderk770/Desktop/R/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_train.csv")
print(head(df.train))
print('   ')
print(str(df.train))

help("missmap")

missmap(df.train,main='Missing Map', col = c('yellow','black'), legend = FALSE)
print(ggplot(df.train,aes(Survived))+ geom_bar())
print(ggplot(df.train,aes(Pclass))+ geom_bar(aes(fill=factor(Pclass))))
print(ggplot(df.train,aes(Sex))+ geom_bar(aes(fill=factor(Sex))))
print(ggplot(df.train,aes(Age))+ geom_histogram(bins=20,alpha=0.5,fill='blue'))
print(ggplot(df.train,aes(SibSp))+ geom_bar())
print(ggplot(df.train,aes(Fare))+ geom_histogram(bins=20,alpha=0.5,fill='green',color='black'))

pl <- ggplot(df.train, aes(Pclass,Age))
pl<- pl + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
pl+scale_y_continuous(breaks = seq(min(0),max(80),by=2)) + theme_bw()

#function for Imputation of age based on class
impute_age <- function(age,class){
  out<-age
  for(i in 1:length(age)){
    if(is.na(age[i])){
      if(class[i]==1){
        out[i] <- 37
      }else if (class[i]==2){
        out[i]<- 29
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
    return(out)
}
#####
fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
#####
df.train$Age <- fixed.ages
###
missmap(df.train, main='Imputation Check', col=c('yellow','black'),legend=FALSE)

str(df.train)
df.train <- select(df.train, -PassengerId, -Name, -Ticket, -Cabin)
head(df.train)
str(df.train)
#grouping as a factor. int variable 
##choice to make or not.
df.train$Survived <-factor(df.train$Survived)
df.train$Pclass <-factor(df.train$Pclass)
df.train$Parch <-factor(df.train$Parch)                     
df.train$SibSp <-factor(df.train$SibSp)      

#training the model

log.model <- glm(Survived ~., family = binomial(link = 'logit'), data=df.train)
summary(log.model)
set.seed(101)
split<-sample.split(df.train$Survived,SplitRatio =  0.7)
final.train <- subset(df.train,split==TRUE)
final.test <- subset(df.train,split==FALSE)
final.log.model <- glm(Survived~.,family = binomial(link = 'logit'),data = final.train)
summary(final.log.model)

fitted.probabilities <- predict(final.log.model,final.test,type = 'response')
#shorthand if-else function
fitted.results <- ifelse(fitted.probabilities>0.5,1,0) 
missclassError <- mean(fitted.results != final.test$Survived)
print(1-missclassError) #confussion matrix
table(final.test$Survived,fitted.probabilities>0.50)