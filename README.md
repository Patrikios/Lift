# Lift
Calculates and visualises lift as a performance measure for binary classification problems

The following example illustrates the working of the lift function:

```R
library(ranger); library(caret)
data <- fread("https://raw.githubusercontent.com/just4jin/bank-marketing-prediction/master/data/bank_full.csv")
inx <- createDataPartition(data$y, list = F, p = 0.7)
train <- data[inx,]
test <- data[-inx,]
set.seed(23)
model_ranger <- ranger(y ~ ., data = train, probability = T)
pred <- data.table(event = test$y,
                   pred = predictions(predict(model_ranger, test))[, "yes"]
                   )

liftMe(
  dataWithProbabilityPrediction = pred, 
  levelPositive = "yes",
  responseVariable = "event", 
  probabilityOfChurning = "pred"
  
)
```
