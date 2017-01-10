library(neuralnet)
library(nnet)
library(caret)

movieData = read.csv("F:\\Nilam\\Job History\\Quantiphi\\Case Study\\TrainingData.csv")
names(movieData)

predVars = names(movieData) %in% c("id","display_name", "board_rating_reason", "total","production_year")
updatedData = movieData[!predVars]

#Fix College names as row names
rownames(updatedData)=updatedData[,1]
updatedData =updatedData[,-1]
attach(updatedData)

#Data Summmary
str(updatedData)
#updatedData$production_year = as.factor(updatedData$production_year)
updatedData$movie_sequel = as.factor(updatedData$movie_sequel)
updatedData$Category = factor(updatedData$Category,levels=c(1,2,3,4,5,6,7,8,9),ordered=TRUE)

set.seed(12345)

####################### Model 1 ###########################################
nnModel = nnet(Category ~ movie_sequel+creative_type+source+production_method+genre+language+movie_board_rating_display_name+movie_release_pattern_display_name, data = updatedData, size=8,decay = 0.0001, maxit = 10000)

#plot(nnModel, rep= "best")
predTrain = predict(nnModel)
results = as.data.frame(predTrain)

maxVals = (sapply(seq(nrow(results)), function(i) {
  strtoi(colnames(results)[which.max(results[i,])])
}))

confusionMatrix(updatedData$Category, maxVals)

####################### Model 2 ###########################################
updatedData$Category.new = factor(updatedData$Category,levels=c(1,2,3,4,5,6,7,8,9),ordered=TRUE)

updatedData$Category.new[updatedData$Category.new == 2] = 1
updatedData$Category.new[updatedData$Category.new == 3] = 2
updatedData$Category.new[updatedData$Category.new == 4] = 2
updatedData$Category.new[updatedData$Category.new == 5] = 3
updatedData$Category.new[updatedData$Category.new == 6] = 3
updatedData$Category.new[updatedData$Category.new == 7] = 4
updatedData$Category.new[updatedData$Category.new == 8] = 4
updatedData$Category.new[updatedData$Category.new == 9] = 5

updatedData$Category.new = factor(updatedData$Category.new,levels=c(1,2,3,4,5),ordered=TRUE)

#nnModel = neuralnet(modelForm, inputData, hidden = 50, linear.output = FALSE)
nnModel2 = nnet(Category.new ~ movie_sequel+creative_type+source+production_method+genre+language+movie_board_rating_display_name+movie_release_pattern_display_name, data = updatedData, size=8,decay = 0.0001, maxit = 10000)

#plot(nnModel, rep= "best")
library(caret)
predTrain2 = predict(nnModel2)
results = as.data.frame(predTrain2)
#names(results) = c(seq(1:9))
# 
maxVals = (sapply(seq(nrow(results)), function(i) {
  strtoi(colnames(results)[which.max(results[i,])])
}))

confusionMatrix(updatedData$Category.new, maxVals)

####################### Model 3 ###########################################

updatedData$Category.new2 = factor(updatedData$Category,levels=c(1,2,3,4,5,6,7,8,9),ordered=TRUE)

updatedData$Category.new2[updatedData$Category.new2 == 1] = 1
updatedData$Category.new2[updatedData$Category.new2 == 2] = 1
updatedData$Category.new2[updatedData$Category.new2 == 3] = 2
updatedData$Category.new2[updatedData$Category.new2 == 4] = 2
updatedData$Category.new2[updatedData$Category.new2 == 5] = 2
updatedData$Category.new2[updatedData$Category.new2 == 6] = 3
updatedData$Category.new2[updatedData$Category.new2 == 7] = 3
updatedData$Category.new2[updatedData$Category.new2 == 8] = 4
updatedData$Category.new2[updatedData$Category.new2 == 9] = 5

updatedData$Category.new2 = factor(updatedData$Category.new2,levels=c(1,2,3,4,5),ordered=TRUE)

nnModel3 = nnet(Category.new2 ~ movie_sequel+creative_type+source+production_method+genre+language+movie_board_rating_display_name+movie_release_pattern_display_name, data = updatedData, size=8,decay = 0.0001, maxit = 10000)

predTrain3 = predict(nnModel3)
results = as.data.frame(predTrain3)

maxVals = (sapply(seq(nrow(results)), function(i) {
  strtoi(colnames(results)[which.max(results[i,])])
}))

confusionMatrix(updatedData$Category.new2, maxVals)

################################ Scoring ##################################
#Reading test data
testData = read.csv("F:\\Nilam\\Job History\\Quantiphi\\Case Study\\ScoringData.csv")
names(testData)

predVars = names(testData) %in% c("id","display_name", "board_rating_reason", "production_budget","production_year")
testData = testData[!predVars]
#testData$production_year = as.factor(testData$production_year)
testData$movie_sequel = as.factor(testData$movie_sequel)

#Fix College names as row names
rownames(testData)=testData$name
testData =testData[,-which(names(testData) == 'name')]
testData$creative_type = factor(testData$creative_type, levels = levels(updatedData$creative_type))
testData$source = factor(testData$source, levels = levels(updatedData$source))
testData$production_method = factor(testData$production_method, levels = levels(updatedData$production_method))
testData$genre = factor(testData$genre, levels = levels(updatedData$genre))
testData$language = factor(testData$language, levels = levels(updatedData$language))
testData$movie_board_rating_display_name = factor(testData$movie_board_rating_display_name, levels = levels(updatedData$movie_board_rating_display_name))
testData$movie_release_pattern_display_name = factor(testData$movie_release_pattern_display_name, levels = levels(updatedData$movie_release_pattern_display_name))

testData = testData[complete.cases(testData),]

score = predict(nnModel, testData)

testData$predVals = (sapply(seq(nrow(score)), function(i) {
  strtoi(colnames(score)[which.max(score[i,])])
}))

write.csv(testData, "F:\\Nilam\\Job History\\Quantiphi\\Case Study\\NN2.csv")
