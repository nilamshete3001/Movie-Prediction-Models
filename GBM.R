library(caret)

movieData = read.csv("F:\\Nilam\\Job History\\Quantiphi\\Case Study\\TrainingData.csv")
names(movieData)

predVars = names(movieData) %in% c("id","display_name", "board_rating_reason", "total", "production_year")
updatedData = movieData[!predVars]

#Fix College names as row names
rownames(updatedData)=updatedData[,1]
updatedData =updatedData[,-1]

#Data Summmary
str(updatedData)
#updatedData$production_year = as.factor(updatedData$production_year)
updatedData$movie_sequel = as.factor(updatedData$movie_sequel)

updatedData$Category = factor(updatedData$Category,levels=c(1,2,3,4,5,6,7,8,9),ordered=TRUE)

set.seed(12345)
################################### Model1 ############################
bmodel1 = train(Category ~ movie_sequel+creative_type+source+production_method+genre+language+movie_board_rating_display_name+movie_release_pattern_display_name , method = "gbm", data = updatedData, verbose = F, trControl = trainControl(method = "cv", number = 5))

confusionMatrix(predict(bmodel1), updatedData$Category)

plot(bmodel1)
plot(bmodel1,plotType = "level")
resampleHist((bmodel1))

################################### Model2 ############################
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

bmodel2 = train(Category.new ~ movie_sequel+creative_type+source+production_method+genre+language+movie_board_rating_display_name+movie_release_pattern_display_name, method = "gbm", data = updatedData, verbose = F, trControl = trainControl(method = "cv", number = 5))
confusionMatrix(predict(bmodel2), updatedData$Category.new)

plot(bmodel2)
plot(bmodel2,plotType = "level")
resampleHist((bmodel2))

################################## Model 3 ##########################################
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

bmodel3 = train(Category.new2 ~ movie_sequel+creative_type+source+production_method+genre+language+movie_board_rating_display_name+movie_release_pattern_display_name, method = "gbm", data = updatedData, verbose = F, trControl = trainControl(method = "cv", number = 5))

confusionMatrix(predict(bmodel3), updatedData$Category.new2)

plot(bmodel3)
plot(bmodel3,plotType = "level")
resampleHist((bmodel3))

################################## Scoring ##########################################
#Reading test data
testData = read.csv("F:\\Nilam\\Job History\\Quantiphi\\Case Study\\ScoringData.csv")
names(testData)

predVars = names(testData) %in% c("id","display_name", "board_rating_reason","production_budget","production_year")
testData = testData[!predVars]
#testData$production_year = as.factor(testData$production_year)
testData$movie_sequel = as.factor(testData$movie_sequel)

#Fix College names as row names
rownames(testData)=testData[,1]
testData =testData[,-which(names(testData) == 'name')]

#testData$production_year = factor(testData$production_year, levels = levels(updatedData$production_year))
testData$creative_type = factor(testData$creative_type, levels = levels(updatedData$creative_type))
testData$source = factor(testData$source, levels = levels(updatedData$source))
testData$production_method = factor(testData$production_method, levels = levels(updatedData$production_method))
testData$genre = factor(testData$genre, levels = levels(updatedData$genre))
testData$language = factor(testData$language, levels = levels(updatedData$language))
testData$movie_board_rating_display_name = factor(testData$movie_board_rating_display_name, levels = levels(updatedData$movie_board_rating_display_name))
testData$movie_release_pattern_display_name = factor(testData$movie_release_pattern_display_name, levels = levels(updatedData$movie_release_pattern_display_name))

testData = testData[complete.cases(testData),]
testData$pred = predict(bmodel1, testData)

write.csv(testData, "F:\\Nilam\\Job History\\Quantiphi\\Case Study\\boosting2.csv")

