library(dplyr)
library(tidyr)
library(caret)

#Read the data
movieData = read.csv("F:\\Nilam\\Job History\\Quantiphi\\Case Study\\TrainingData.csv")
names(movieData)

#Remove variables
predVars = names(movieData) %in% c("id","display_name", "board_rating_reason", "total","production_year")
updatedData = movieData[!predVars]

#Fix College names as row names
rownames(updatedData)=updatedData[,1]
updatedData =updatedData[,-1]

#Data Summmary
str(updatedData)
#updatedData$production_year = as.factor(updatedData$production_year)
updatedData$movie_sequel = as.factor(updatedData$movie_sequel)
attach(updatedData)

#Pairwise scatterplots
library(psych)
pairs.panels(updatedData)

#VIF Calculation
library(usdm)
X=model.matrix(Category ~ ., data=updatedData)[,-1]
vifstep(X, th=10)

#Basic model fit
fit=lm(Category ~ ., data=updatedData)
summary(fit)

#Accuracy : 0.2717 
confusionMatrix(round(fit$fitted.values, digits = 0), Category)

#Since in language variable none of the categgory is coming significant we can remove that variable and check model fit
updatedFit=lm(Category ~ . - language, data=updatedData)

#After removing language varible adjusted R-square has increased and also RMSE value has decreased
summary(updatedFit)

#Accuracy : 0.2726
confusionMatrix(round(updatedFit$fitted.values, digits = 0), Category)

#Fit stepwise regression
stepModel = step(fit, direction = "both", k = 2)
stepFit = lm(Category ~ movie_sequel + creative_type + source + 
            production_method + genre + movie_board_rating_display_name + 
            movie_release_pattern_display_name, data=updatedData)

summary(stepFit)

#Accuracy : 0.2625
confusionMatrix(round(stepFit$fitted.values, digits = 0), Category)

#Residual Analysis
par(mfrow=c(2,2))
plot(stepFit)

#Find out outlier observation
which(cooks.distance(stepFit) >= 0.03)
par(mfrow=c(1,1))
plot(stepFit, which=c(4))

#Remove outlier observations
updatedData.new = updatedData[-(which(cooks.distance(stepFit) >= 0.03)),]

newFit= lm(Category ~ movie_sequel + creative_type + source + 
            production_method + genre + movie_board_rating_display_name + 
            movie_release_pattern_display_name, data=updatedData.new)

summary(newFit)

#Accuracy : 0.2668
confusionMatrix(round(newFit$fitted.values, digits = 0), updatedData.new$Category)

#Reading test data
testData = read.csv("F:\\Nilam\\Job History\\Quantiphi\\Case Study\\ScoringData.csv")
names(testData)

predVars = names(testData) %in% c("id","display_name", "board_rating_reason","production_year")
testData = testData[!predVars]

#Fix College names as row names
rownames(testData)=testData[,1]
testData =testData[,-1]
#testData$production_year = as.factor(testData$production_year)
testData$movie_sequel = as.factor(testData$movie_sequel)

testData$creative_type = factor(testData$creative_type, levels = levels(updatedData$creative_type))
testData$source = factor(testData$source, levels = levels(updatedData$source))
testData$production_method = factor(testData$production_method, levels = levels(updatedData$production_method))
testData$genre = factor(testData$genre, levels = levels(updatedData$genre))
testData$language = factor(testData$language, levels = levels(updatedData$language))
testData$movie_board_rating_display_name = factor(testData$movie_board_rating_display_name, levels = levels(updatedData$movie_board_rating_display_name))
testData$movie_release_pattern_display_name = factor(testData$movie_release_pattern_display_name, levels = levels(updatedData$movie_release_pattern_display_name))

testData = testData[complete.cases(testData),]
testData$pred = round(predict(newFit,testData), digits = 0)

write.csv(testData, "F:\\Nilam\\Job History\\Quantiphi\\Case Study\\LR.csv")
