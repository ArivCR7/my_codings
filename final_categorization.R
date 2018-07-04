install.packages("data.table")
install.packages("textstem")
library(textstem)
library(data.table)
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
library(tm)
library(SnowballC)
library(wordcloud)

#reading correct data
rm(correct_data)
rm(wrong_data)
correct_data<-fread(file = "valid.csv",header = TRUE)
correct_data<-correct_data[,c("description")]
correct_data<-as.data.frame(correct_data)
rm(correct)
correct<-correct_data[,c("description")]
correct<-gsub("<.*?>","",correct)
View(correct)
class(correct)
correct_dat<-as.data.frame(correct)
correct_dat1<-as.data.frame(correct_dat[which(correct_dat$description!=""),])
View(correct_dat1)
colnames(correct_dat1)<-"description"
wrong_data<-fread(file = "invalid.csv",header = TRUE)
wrong_data<-wrong_data[,c("description")]
library(plyr)
train_data<-rbind.fill(correct_dat1,wrong_data)
nrow(train_data)
class(train_data)
test_data<-fread(file = "daily.csv",header = TRUE)
train<-train_data[,c("description")]
class(train)
test_data<-as.data.frame(test_data)
test<-test_data[,c("description")]
train<-gsub("<.*?>","",train)
class(train)
View(train)

library(tm)
train_corp <- VCorpus(VectorSource(train))
train_corp <- tm_map(train_corp, content_transformer(stripWhitespace))
train_corp <- tm_map(train_corp, content_transformer(tolower))
#train_corp <- tm_map(train_corp, content_transformer(removeNumbers))
train_corp <- tm_map(train_corp, content_transformer(removePunctuation))
train_corp <- tm_map(train_corp,removeWords,stopwords('english'))
train_corp <- tm_map(train_corp, content_transformer(stemDocument))
#train_corp <- tm_map(train_corp, content_transformer(lemmatize_strings))

inspect(train_corp[[2566]])

#Preparing dtm for unigrams
uni_train.dtm <-DocumentTermMatrix(train_corp,control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
uni_train.dtm
uni_train.dtm<- removeSparseTerms(uni_train.dtm,0.995)
uni_train.dtm<-as.matrix(uni_train.dtm)

#Doing same for test
rm(test)
test<-test_data[,c("description")]
View(test)
test<-gsub("<.*?>","",test)
test_corp<-VCorpus(VectorSource(test))
test_corp
test_corp <- tm_map(test_corp, content_transformer(stripWhitespace))
test_corp <- tm_map(test_corp, content_transformer(tolower))
#test_corp <- tm_map(test_corp, content_transformer(removeNumbers))
test_corp <- tm_map(test_corp, content_transformer(removePunctuation))
test_corp <- tm_map(test_corp,removeWords,stopwords('english'))
test_corp <- tm_map(test_corp, content_transformer(stemDocument))
#test_corp <- tm_map(test_corp, content_transformer(lemmatize_strings))
inspect(test_corp[[3]])
uni_test.dtm <-DocumentTermMatrix(test_corp,control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
uni_test.dtm
uni_test.dtm<-as.matrix(uni_test.dtm)

#Keeping columns common in both train and test
ncol(uni_train.dtm)
ncol(uni_test.dtm)
colnames(uni_train.dtm)
colnames(uni_test.dtm)
uni_train.df<-data.frame(uni_train.dtm[,intersect(colnames(uni_train.dtm),colnames(uni_test.dtm))])
uni_test.df<-data.frame(uni_test.dtm[,intersect(colnames(uni_test.dtm),colnames(uni_train.dtm))])
View(uni_test.df)

BigramTokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
bi_train.dtm <- DocumentTermMatrix(train_corp, control = list(tokenize = BigramTokenizer))
bi_train.dtm<-removeSparseTerms(bi_train.dtm,0.995)
bi_train.dtm<-as.matrix(bi_train.dtm)
bi_test.dtm <- DocumentTermMatrix(test_corp, control = list(tokenize = BigramTokenizer))
bi_test.dtm<-removeSparseTerms(bi_test.dtm,0.995)
bi_test.dtm
bi_test.dtm<-as.matrix(bi_test.dtm)
bi_train.df<-data.frame(bi_train.dtm[,intersect(colnames(bi_train.dtm),colnames(bi_test.dtm))])
bi_test.df<-data.frame(bi_test.dtm[,intersect(colnames(bi_test.dtm),colnames(bi_train.dtm))])

#Merging both unigrams and bigram features
train.df<-cbind(uni_train.df,bi_train.df)
test.df<-cbind(uni_test.df,bi_test.df)

train.df$`y_variable` <- "valid"
train.df$y_variable[4331:nrow(train.df)]<-"invalid"
train.df$y_variable<-factor(train.df$y_variable)

rf.model<-randomForest(y_variable~.,data = train.df,ntrees=100,mtry=10,importance=TRUE,na.action = NULL)
rf.pred.prob<-predict(rf.model,newdata = test.df,type = "prob")

uni_train.df$`y_variable` <- "valid"
uni_train.df$y_variable[4004:nrow(uni_train.df)]<-"invalid"
uni_train.df$y_variable<-factor(uni_train.df$y_variable)
library(randomForest)
rf.model<-randomForest(y_variable~.,data = uni_train.df,ntrees=100,mtry=10,importance=TRUE)
rf.pred.prob<-predict(rf.model,newdata = uni_test.df,type = "prob")
nrow(rf.pred.prob)
View(train)
head(rf.pred.prob)
sum(rf.pred.prob[,2]>0.95)
sum(rf.pred.prob[,1]>0.95)
test.df$predicted<-""
test.df$predicted[which(rf.pred.prob[,1]>0.95)]<-"invalid"
test.df$predicted[which(rf.pred.prob[,2]>0.95)]<-"valid"
View(test.df)
library(dplyr)
test_data$predicted<-""
test_data$predicted<-test.df$predicted
class(test_data)
write.csv(test_data,"output.csv")

