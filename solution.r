
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for plotting 
library(corrplot)   # used for making correlation plot
library(cowplot)    # used for combining multiple plots    
library(rpart)      # used for decision tree regression
library(MLmetrics)  # used for measuring performance of models
library(randomForest)  # used for random forest regression
library(caTools) # used for splitting data
library(stringr) # used for String manipulation
library(tidyr) # used for data cleaning
library(tm) # used for text mining
library(SnowballC) # used for converting words to root word
library(wordcloud) # used for creating wordcloud

train = read.csv("train.csv",stringsAsFactors = T) #importing the data.

head(train)

dim(train)

str(train)

ggplot(train %>% group_by(review.overall) %>% summarise(Count = n())) +
geom_bar(aes(review.overall, Count), stat = "identity", fill = "coral1")

p1 = ggplot(train %>% group_by(review.aroma) %>% summarise(Count = n())) +
geom_bar(aes(review.aroma, Count), stat = "identity", fill = "blue")+
geom_label(aes(review.aroma, Count, label = Count), vjust = 0.5)

p2 = ggplot(train %>% group_by(review.palate) %>% summarise(Count = n())) +
geom_bar(aes(review.palate, Count), stat = "identity", fill = "blue")+
geom_label(aes(review.palate, Count, label = Count), vjust = 0.5)

p3 = ggplot(train %>% group_by(review.appearance) %>% summarise(Count = n())) +
geom_bar(aes(review.appearance, Count), stat = "identity", fill = "blue")+
geom_label(aes(review.appearance, Count, label = Count), vjust = 0.5)

plot_grid(p1,p2,p3,nrow = 1)

p4 = ggplot(train,aes(train$beer.ABV))+geom_histogram(binwidth = 0.5,fill = "Green")+xlab("Beer ABV")
p5 = ggplot(train) + geom_histogram(aes(beer.beerId), bins = 100,fill = "Green")+xlab("BeerID")
p6 = ggplot(train,aes(train$beer.brewerId))+geom_histogram(binwidth = 1,fill = "Green")+xlab("BrewerID")+scale_x_log10()


plot_grid(p4,p5,p6,nrow = 1)

p7 = ggplot(train %>% group_by(review.taste) %>% summarise(Count = n()),aes(review.taste, Count)) +
geom_bar( stat = "identity", fill = "coral1") +
xlab("Review Taste") 

p8 = ggplot(train %>% group_by(user.gender) %>% summarise(Count = n()),aes(user.gender, Count)) +
geom_bar( stat = "identity", fill = "coral1") +
xlab("User Gender")+scale_y_log10()

second_row = plot_grid(p8, nrow = 1)
plot_grid(p7, second_row, ncol = 1)

summary(is.na(train))

train = train[,c(2,5,6,7,8,9,10,11,12,13)]

head(train)

style = train$beer.style

train$beer.style = as.integer(train$beer.style)

train$beer.style = as.factor(train$beer.style)

summary(train$beer.style)

name = train$beer.name
train$beer.name = as.integer(train$beer.name)
train$beer.name = as.factor(train$beer.name)
summary(train$beer.name)

month = c()
for(i in 1:length(train$review.timeStruct)){
m=train$review.timeStruct[i]
m = (str_extract_all(m, "(?<=\\{).+?(?=\\})")[[1]])
m = str_split_fixed(m,",",9)
as.list(m)
m[7]
month = rbind(month,as.integer(str_split_fixed(m[7],":",2)[[2]]))
    }
month = as.factor(month)
length(month)

train = cbind(train,month)

train = train[,-c(10)] #removing review.timeStruct column

str(train)

data = train[,c(6,9)]

head(data)

corpous = VCorpus(VectorSource(data$review.text))
corpous = tm_map(corpous,content_transformer(tolower)) # converting all text to lower alphabets
corpous = tm_map(corpous,removeNumbers)# removing all numeric data 
corpous = tm_map(corpous,removePunctuation)# removing all punctuation marks
corpous = tm_map(corpous,removeWords,stopwords()) # removing extra words like pronouns, articles etc.
corpous = tm_map(corpous,stemDocument)# converting all words to there root words
corpous = tm_map(corpous,stripWhitespace) # removing white spaces

dtm = DocumentTermMatrix(corpous)# creating sparse matrix
dtm = removeSparseTerms(dtm,0.98)
dtm

dtmm = TermDocumentMatrix(corpous)
dtmm = removeSparseTerms(dtmm,0.98)
m = as.matrix(dtmm)
v = sort(rowSums(m),decreasing = TRUE)
d = data.frame(word = names(v),freq = v)
wordcloud(words = d$word,freq = d$freq,colors = brewer.pal(8,"Dark2"))

dim(dtm)

dataset = as.data.frame(as.matrix(dtm))#converting again as data frame
dataset$review.overall = data$review.overall

dim(dataset)

set.seed(999)
split = sample.split(dataset$review.overall, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

regressor1 = randomForest(training_set[,],training_set$review.overall,ntree = 10)

reviewByText = predict(regressor1, dataset[])
head(reviewByText)

train = cbind(train,reviewByText)
train = train[,-c(9)] #removing review.text column 

cor_train = cor(train[,-c(2,3,6,9)]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

set.seed(999)
split = sample.split(train$review.overall, SplitRatio = 0.8)
training_set = subset(train, split == TRUE)
test_set = subset(train, split == FALSE)

regressor = rpart(formula = review.overall ~ .,
                  data = training_set[],
                  control = rpart.control(minsplit = 4))

y_pred = predict(regressor, test_set[])

R2_Score(y_pred,test_set$review.overall) # prediction on Test dataset

MAE(y_pred,test_set$review.overall)
MSE(y_pred,test_set$review.overall)
RMSE(y_pred,test_set$review.overall)

y_pred1 = predict(regressor,training_set) # Prediction on train dataset.
R2_Score(y_pred1,training_set$review.overall)

regressor1 = randomForest(training_set[,-c(2,3)],training_set$review.overall,ntree = 100)

y_pred3 = predict(regressor1, test_set[]) # Prediction on test dataset.

R2_Score(y_pred3,test_set$review.overall)

y_pred4 = predict(regressor1,training_set) # Prediction on train dataset.
R2_Score(y_pred4,training_set$review.overall)

folds = createFolds(training_set$review.overall, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
   regressor = rpart(formula = review.overall ~ .,
                  data = training_fold,
                  control = rpart.control(minsplit = 4))
  y_pred = predict(regressor, newdata = test_fold[])
  accuracy = R2_Score(y_pred,test_fold$review.overall)
  return(accuracy)
})
cv
