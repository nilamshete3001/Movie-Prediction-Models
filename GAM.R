library(MASS)

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

lapply(updatedData[, c("Category", "creative_type", "genre")], table)
data = ftable(xtabs(Category ~ genre + creative_type, data = updatedData))

ggplot(updatedData, aes(x = creative_type, y = Category)) +
  geom_boxplot(size = 0.75) +
  geom_jitter(alpha = .5) +
  facet_grid( ~ genre, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

library(MASS)
str(updatedData)
updatedData$Category = factor(updatedData$Category,levels=c(1,2,3,4,5,6,7,8,9),ordered=TRUE)

model = polr(Category ~ . - Category, data = updatedData, Hess=TRUE)
summary(model)

coefTable = coef(summary(model))
pvalue = pnorm(abs(coefTable[, "t value"]), lower.tail = FALSE) * 2
coefTable = cbind(coefTable, "p_value" = pvalue)

model1 = update(model, method = "probit", Hess = TRUE)#Lowest
summary(update(model, method = "logistic", Hess = TRUE), digits = 3) #4145.527
summary(update(model, method = "loglog", Hess = TRUE), digits = 3) #4145.527

model2 <- stepAIC(model1, ~.^2)
polr1 = polr(Category ~ movie_sequel + creative_type + source + 
               production_method + genre + movie_board_rating_display_name + 
               movie_release_pattern_display_name + movie_sequel:movie_board_rating_display_name + 
               movie_sequel:source + creative_type:production_method + production_method:movie_board_rating_display_name, 
             data = updatedData, Hess = TRUE, method = "probit")
mmodel1 <- polr1$model
mmodel1$modelp1 <- predict(polr1)

library(caret)
confusionMatrix(mmodel1$modelp1, mmodel1$Category)

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

model3 = polr(updatedData$Category.new ~ . - Category, data = updatedData, Hess = TRUE, method = "probit") #2512.727 
summary(update(model3, method = "probit", Hess = TRUE), digits = 3) #2511.983 
summary(update(model3, method = "logistic", Hess = TRUE), digits = 3)
summary(update(model3, method = "loglog", Hess = TRUE), digits = 3)

modelStep <- stepAIC(model3, ~.^2)

model4 = polr(formula = Category.new ~ movie_sequel + creative_type + 
                source + production_method + genre + movie_board_rating_display_name + 
                movie_release_pattern_display_name + movie_sequel:movie_board_rating_display_name + 
                movie_sequel:source + creative_type:production_method + production_method:movie_board_rating_display_name, 
              data = updatedData, Hess = TRUE, method = "probit", control = list(maxit = 5000))

(ctable <- coef(summary(model4)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

mmodel2 <- model4$model
mmodel2$modelp1 <- predict(model4)

confusionMatrix(mmodel2$modelp1, mmodel2$Category.new)

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

model5 = polr(updatedData$Category.new2 ~ . - Category -Category.new, method = "probit", data = updatedData, Hess = TRUE) #2161 
summary(update(model5, method = "logistic", Hess = TRUE), digits = 3)
summary(update(model5, method = "loglog", Hess = TRUE), digits = 3) #2205

modelStep2 <- stepAIC(model5, ~.^2)
model6 = polr(formula = Category.new2 ~ movie_sequel + creative_type + 
                source + production_method + genre + movie_board_rating_display_name + 
                movie_release_pattern_display_name + movie_sequel:source + 
                creative_type:production_method + movie_sequel:movie_board_rating_display_name, 
              data = updatedData, Hess = TRUE, method = "probit")

mmodel3 <- model6$model
mmodel3$modelp1 <- predict(model3)

confusionMatrix(mmodel3$modelp1, mmodel3$Category.new2)


#Reading test data
testData = read.csv("F:\\Nilam\\Job History\\Quantiphi\\Case Study\\ScoringData.csv")
names(testData)

predVars = names(testData) %in% c("id","display_name", "board_rating_reason","production_budget", "production_year")
testData = testData[!predVars]
testData$movie_sequel = as.factor(testData$movie_sequel)

#Fix College names as row names
rownames(testData)=testData[,1]
testData =testData[,-which(names(testData) == 'name')]

testData$creative_type = factor(testData$creative_type, levels = levels(updatedData$creative_type))
testData$source = factor(testData$source, levels = levels(updatedData$source))
testData$production_method = factor(testData$production_method, levels = levels(updatedData$production_method))
testData$genre = factor(testData$genre, levels = levels(updatedData$genre))
testData$language = factor(testData$language, levels = levels(updatedData$language))
testData$movie_board_rating_display_name = factor(testData$movie_board_rating_display_name, levels = levels(updatedData$movie_board_rating_display_name))
testData$movie_release_pattern_display_name = factor(testData$movie_release_pattern_display_name, levels = levels(updatedData$movie_release_pattern_display_name))

testData = testData[complete.cases(testData),]

pred = predict(model6, testData, type ="probs")
