# Author: Julien Diot juliendiot@ut-biomet.org
# 2019 The University of Tokyo
#
# Description:
# A simple KNN model for the iris dataset.

#### PACKAGES ####
library(caret)


#### OPTIONS ####
# options(stringsAsFactors = FALSE)


#### CODE ####

# Import data
data(iris)


# train model
trControl <- trainControl(method = "boot",
                          number = 100)
method <- "knn"

(irisKNNmodel <- train(
  Species ~ .,
  data = iris,
  method = method,
  tuneGrid = data.frame(k = seq(1, 50, by = 2)),
  # tuneLength = 10,
  trControl = trControl))
# ggplot(irisKNNmodel, metric = "Accuracy")

saveRDS(irisKNNmodel, file = "data/irisKNNmodel.rds")
