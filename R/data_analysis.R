# install.packages("caret")
library(caret)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("dplyr")
library(dplyr)

data <- read.csv("all_data.csv", stringsAsFactors = FALSE)
data$artist_name <- data$track_name <- NULL
data <- data[complete.cases(data), ]

data <- data %>%
    group_by(genre) %>%
    sample_n(3000)

pca <- prcomp(data[ , ! colnames(data) %in% "genre"], center = TRUE, scale. = TRUE)
pca <- data.frame(pca$x[ , 1:2], genre=data$genre)

ggplot(pca, aes(PC1, PC2, color=genre)) + geom_point(size=2, alpha=0.3)

set.seed(2020)
train_ind <- sample(nrow(data), floor(nrow(data) * 0.8), FALSE)
train_data <- data[train_ind, ]
test_data <- data[-train_ind, ]

tr <- trainControl(method='cv', number=10)


knnmodel <- train(genre ~ ., data=train_data, method='knn', tuneGrid=expand.grid(.k=seq(1, 70, 2)), 
                  metric='Accuracy', trControl=tr)
plot(knnmodel)
confusionMatrix(knnmodel)
knnmodel
mean(predict(knnmodel, test_data) == test_data$genre)


rfmodel <- train(genre ~ ., data=train_data, method='rf', 
                  metric='Accuracy', trControl=tr)
plot(rfmodel)
confusionMatrix(rfmodel)
rfmodel
mean(predict(rfmodel, test_data) == test_data$genre)


svmmodel <- train(genre ~ ., data=train_data, method='svmRadial', 
                 metric='Accuracy', trControl=tr)
plot(svmmodel)
confusionMatrix(svmmodel)
svmmodel
mean(predict(svmmodel, test_data) == test_data$genre)


nbmodel <- train(genre ~ ., data=train_data, method='nb', 
                  metric='Accuracy', trControl=tr)
plot(nbmodel)
confusionMatrix(nbmodel)
nbmodel
mean(predict(nbmodel, test_data) == test_data$genre)


nnmodel <- train(genre ~ ., data=train_data, method='nnet', 
                 metric='Accuracy', trControl=tr)
plot(nnmodel)
confusionMatrix(nnmodel)
nnmodel
mean(predict(nnmodel, test_data) == test_data$genre)


resamps <- resamples(list(knn = knnmodel, rf = rfmodel, 
                          svm = svmmodel, nb = nbmodel, nn = nnmodel))
summary(resamps)
dotplot(resamps, metric = "Accuracy")

plot(varImp(rfmodel))

library(reshape2)
train_data_long <- melt(train_data)
ggplot(train_data_long, aes(value, fill=genre)) + 
    geom_density(alpha=0.5) + 
    facet_wrap(~variable, scales="free")



png("all_variables_train.png", units="in", width=14, height=10, res=800)
dev.off()
