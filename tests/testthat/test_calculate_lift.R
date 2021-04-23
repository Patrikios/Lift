data <- fread("https://raw.githubusercontent.com/just4jin/bank-marketing-prediction/master/data/bank_full.csv")
inx <- createDataPartition(data$y, list = F, p = 0.7)
train <- data[inx,]
test <- data[-inx,]
set.seed(23)
model_ranger <- ranger(as.factor(y) ~ ., data = train, probability = T)
pred <- data.table(event = test$y,
                   pred = predictions(predict(model_ranger, test))[, "yes"]
)
ex <- expression(calculate_lift(
  dataWithProbabilityPrediction = pred, 
  levelPositive = "yes",
  responseVariable = "event", 
  probabilityOfChurning = "pred"
))


test_that("Returned is data.frame", {
  expect_true( is.data.frame(eval(ex)) )
})

test_that("Returned is data.frame", {
  expect_message( eval(ex), 'Positive event rate')
})
