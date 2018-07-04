setwd("C:/Users/a.ezhilarasan/Documents/AI/R/SIT")
library(data.table)
file.remove("output.csv")
correct_data<-fread(file = "valid_with_desc.csv",header = TRUE)
wrong_data<-fread(file = "invalid_with_desc.csv",header = TRUE)
library(plyr)
train_data<-rbind(correct_data,wrong_data)
library(tidyr)
new_train<-as.data.frame(unite(train_data, "desc", c("Description (Customer visible)","Short description (Knowledge search)"),sep=" "))
train<-new_train[,c("desc")]
train<-gsub("<.*?>","",train)
#Removing cirumflex character/ non-ASCII characters
train<-gsub('[^ -~]', '', train)

library(textstem)
lemma_train <- lemmatize_strings(train)
head(lemma_train)

library(tm)
train_corp <- VCorpus(VectorSource(lemma_train))
train_corp <- tm_map(train_corp, content_transformer(stripWhitespace))
train_corp <- tm_map(train_corp, content_transformer(tolower))
train_corp <- tm_map(train_corp, content_transformer(removeNumbers))
train_corp <- tm_map(train_corp, content_transformer(removePunctuation))
train_corp <- tm_map(train_corp,removeWords,stopwords('english'))
#train_corp <- tm_map(train_corp, content_transformer(stemDocument))
#train_corp <- tm_map(train_corp, content_transformer(lemmatize_words))
inspect(train_corp[[35]])

uni_train.dtm <-DocumentTermMatrix(train_corp,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
uni_train.dtm<- removeSparseTerms(uni_train.dtm,0.98)
uni_train.dtm<-as.matrix(uni_train.dtm)
uni_train.dtm
test_data<-fread(file = "assignment.csv",header = TRUE)
new_test<-as.data.frame(unite(test_data, "desc", c("Description (Customer visible)","Short description (Knowledge search)"),sep=" "))
test<-new_test[,c("desc")]
test<-gsub("<.*?>","",test)
#Removing cirumflex character/ non-ASCII characters
test<-gsub('[^ -~]', '', test)
class(test)
head(test)

lemma_test <- lemmatize_words(test)

test_corp <- VCorpus(VectorSource(lemma_test))
test_corp <- tm_map(test_corp, content_transformer(stripWhitespace))
test_corp <- tm_map(test_corp, content_transformer(tolower))
test_corp <- tm_map(test_corp, content_transformer(removeNumbers))
test_corp <- tm_map(test_corp, content_transformer(removePunctuation))
test_corp <- tm_map(test_corp,removeWords,stopwords('english'))
#test_corp <- tm_map(test_corp, content_transformer(stemDocument))
#test_corp <- tm_map(test_corp, content_transformer(lemmatize_words))
#test_corp
inspect(test_corp[[1]])
#test_char<-as.character(test_corp)
#lemma_test <- lemmatize_strings(test_char)
test_corp <- VCorpus(VectorSource(lemma_test))
uni_test.dtm <-DocumentTermMatrix(test_corp,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
uni_test.dtm
uni_test.dtm<- removeSparseTerms(uni_test.dtm,0.9)
uni_test.dtm<-as.matrix(uni_test.dtm)

#Keeping columns common in both train and test
ncol(uni_train.dtm)
ncol(uni_test.dtm)
uni_train.df<-data.frame(uni_train.dtm[,intersect(colnames(uni_train.dtm),colnames(uni_test.dtm))])
dim(uni_train.df)
View(uni_train.df)
uni_test.df<-data.frame(uni_test.dtm[,intersect(colnames(uni_test.dtm),colnames(uni_train.dtm))])
uni_train.df$`y_variable` <- "valid"
uni_train.df$y_variable[(nrow(correct_data)+1):nrow(uni_train.df)]<-"invalid"



uni_train.df$y_variable<-factor(uni_train.df$y_variable)
library(randomForest)
rf.model<-randomForest(y_variable~.,data = uni_train.df,ntrees=100,mtry=4,importance=TRUE)
#rf.pred.prob<-predict(rf.model,newdata = uni_test.df)
rf.pred.prob<-predict(rf.model,newdata = uni_test.df,type = "prob")
head(rf.pred.prob)
index <- which(rf.pred.prob[,1]>0.65)
index
uniq_index<-unique(index)
uniq_index
test_data$prediction<-""
test_data$prediction[uniq_index]<-"invalid"
test_data$prediction[-c(uniq_index)]<-"valid"
View(test_data)
write.csv(test_data,file = "output.csv")


library(class)
knn_modeldata <- uni_train.df[,!colnames(uni_train.df) %in% "y_variable"]
knn.pred <- knn(knn_modeldata, uni_test.df,factor(uni_train.df$y_variable),prob = TRUE)

library(e1071)
svm.model<-svm(y_variable~.,data = uni_train.df,probability = TRUE)
svm.predict<-predict(svm.model,uni_test.df,type = "prob")
table(svm.predict)
head(svm.predict)
which(svm.predict=="invalid")
print(index)

#PCA for training set
inc.pca<-prcomp(uni_train.df, center = TRUE,scale. = TRUE)
print(inc.pca)
plot(inc.pca,type = 'l')
a<-predict(inc.pca,newdata = uni_train.df)
class(a)
train.df<-as.data.frame(a)
train.df$target<-""
train.df$target[1:nrow(correct_data)]<-"valid"

train.df$target[(nrow(correct_data)+1):nrow(train.df)]<-"invalid"
train.df$target<-factor(train.df$target)
library(randomForest)
rf.model<-randomForest(target~.,data = train.df,ntrees=100,mtry=5,importance=TRUE)
#rf.pred.prob<-predict(rf.model,newdata = uni_test.df)
#PCA for training set
test.pca<-prcomp(uni_test.df,center = TRUE, scale. = TRUE)
plot(test.pca,type = 'l')
b <- predict(test.pca,newdata = uni_test.df)
test.df <- as.data.frame(b)

rf.pred.prob<-predict(rf.model,newdata = test.df,type = "prob")
head(rf.pred.prob)
tail(rf.pred.prob)
index <- which(rf.pred.prob[,2]>0.5)
index
uniq_index<-unique(index)
uniq_index

knn_modeldata <- train.df[,!colnames(train.df) %in% "target"]
knn.pred <- knn(knn_modeldata, test.df,factor(train.df$target))
