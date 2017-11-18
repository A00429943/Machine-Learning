adult <- read.csv("C:/Users/maninderk770/Desktop/R/R-Course-HTML-Notes/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv")
head(adult)
library(dplyr)
adult<- select(adult,-x)
print(str(adult))
print(summary(adult))
table(adult$type_employer)

#### Data Cleaning

#combine employer type

unemp <- function(job){
  job <- as.character(job)
  if(job=='Never-worked' | job== 'Without-pay'){
    return('Unemployed')
  }else if(job=='Local-gov' | job== 'State-gov'){
    return('Sl-gov')
  }else if(job=='Self-emp-inc' | job== 'Self-emp-not-inc'){
    return('Self-emp')
  }else{
    return(job)
  }
}
  
  ##Apply
  
  adult$type_employer <- sapply(adult$type_employer, unemp)
  ##
  print(table(adult$type_employer))
  
  ##Marital status
  ##
  table(adult$marital)
    
  
  group_marital <- function(mar){
    mar <- as.character(mar)
    if(mar=='Separated' | mar== 'Divorced' | mar=='Widowed'){
      return('Not-Married')
    }else if(mar=='Never-married'){
      return('mar')
    }
    else{
      return('Married')
    }
  }
  adult$marital <- sapply(adult$marital,group_marital)
  
  print(table(adult$marital))
  
  ## Country
  
  table(adult$country)
  
  Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
            'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
  
  North.America <- c('Canada','United-States','Puerto-Rico' )
  
  Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
              'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
  
  Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                               'El-Salvador','Guatemala','Haiti','Honduras',
                               'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                               'Jamaica','Trinadad&Tobago')
  Other <- c('South')
  
  group_country <- function(ctry){
    if(ctry %in% Asia){
      return('Asia')
    }else if(ctry %in% North.America){
      return('North.America')
    }else if(ctry %in% Europe){
      return('Europe')
    }else if(ctry %in% Latin.and.South.America){
      return('Latin.and.South.America')
    }else{
      return('Other')
    }
  }
  
  adult$country <- sapply(adult$country,group_country)
  print(table(adult$country))
  

  
  ##missing data
  
  library(Amelia)
  
  ##
  adult[adult=='?'] <- NA
  ##
  print(table(adult$type_employer))
  
  ## factoring all the columns changed to character columns
  str(adult)
  adult$type_employer <- sapply(adult$type_employer,factor)
  adult$country <- sapply(adult$country,factor)
  adult$marital <- sapply(adult$marital,factor)
  
  print(table(adult$type_employer))
  
 #missingvalues
   missmap(adult)
   missmap(adult,y.at =c(1),y.labels = c(''),col = c('yellow','black'))
  ##drop missing data
   
   adult<- na.omit(adult)
   ##
   missmap(adult,y.at =c(1),y.labels = c(''),col = c('yellow','black'))
   ##
   library(ggplot2)
   ggplot(adult,aes(age))+geom_histogram(aes(fill=income),color='black',bandwidth=1)+ theme_bw()
   ggplot(adult,aes(hr_per_week))+geom_histogram()+ theme_bw()
   
  #rename column
    head(adult)
    adult<-rename(adult,region=country)
pl<-ggplot(adult,aes(region))+geom_bar(aes(fill=income),color='black')+theme_bw()  

###############################
###Logistic regreesion model###
###############################

head(adult)

#Train Test Split
library(caTools)
#
set.seed(101)
#
sample<-sample.split(adult$income,SplitRatio =0.7)
#Train
train<-subset(adult,sample==T)
#Test
test<-subset(adult,sample==F)
model <- glm(income ~. ,family=binomial(link='logit'),data=train)
summary(model)
new.step.model<-step(model)
Summary(new.step.model)
test$predicted.income <- predict(model,newdata = test,type = 'response')
table(test$income,test$predicted.income>0.5)
acc <- (6374+1421)/(6374+546+874+1421)
acc
recall <- 6374/(6374+546)
recall
percision <- 6374/(6374+874)
percision
