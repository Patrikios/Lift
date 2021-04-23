#' @title calculate_lift
#'
#' @description Provides an overview of the binary classification (Yes, No style) model performance and plots the lift plot
#'
#' @param dataWithProbabilityPrediction A data set object with predictions (probabilities)
#' @param levelPositive Character, positive categorical true value of the binary outcome variable (e.g., 'Y', 'yes' or 'churned')
#' @param responseVariable Character column indicating the response variable  (e.g., 'churn')
#' @param probabilityOfChurning Character column indicating the column, where the prediction probabilities are saves  (e.g., 'predictions')
#'
#' @return A data frame with the Lift values as compared to the base line performance
#' @examples
#' library(data.table)
#' library(caret)
#' library(ranger)
#' data <- fread("https://raw.githubusercontent.com/just4jin/bank-marketing-prediction/master/data/bank_full.csv")
#' inx <- createDataPartition(data$y, list = FALSE, p = 0.7)
#' train <- data[inx, ]
#' test <- data[-inx, ]
#' set.seed(23)
#' model_ranger <- ranger(as.factor(y) ~ ., data = train, probability = TRUE)
#' pred <- data.table(
#'   event = test$y,
#'   pred = predictions(predict(model_ranger, test))[, "yes"]
#' )
#'
#' calculate_lift(
#'   dataWithProbabilityPrediction = pred,
#'   levelPositive = "yes",
#'   responseVariable = "event",
#'   probabilityOfChurning = "pred"
#' )
#' @importFrom scales "percent"
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggplot2 "geom_line"
#' @importFrom ggplot2 "aes"
#' @importFrom ggplot2 "geom_text"
#' @importFrom ggplot2 "theme_minimal"
#' @importFrom ggplot2 "theme"
#' @importFrom ggplot2 "element_text"
#' @importFrom ggplot2 "ggtitle"
#' @importFrom stats "quantile"
#' @importFrom data.table "data.table"
#' @importFrom data.table "fread"
#' @importFrom data.table "fifelse"
#' @importFrom data.table "setorderv"
#' @importFrom data.table "rbindlist"
#' @importFrom data.table "setcolorder"
#' @importFrom ranger "ranger"
#' @importFrom ranger "predictions"
#' @importFrom caret createDataPartition
#' @import curl


# lift calculation for binary response
#' @export
calculate_lift <- function(dataWithProbabilityPrediction = pred,
                           levelPositive = "yes",
                           responseVariable = "y",
                           probabilityOfChurning = "pred") {

  # probability data table and its proxy
  dataWithProbabilityPrediction <- data.table(
    label = dataWithProbabilityPrediction[[responseVariable]],
    probabilityOfEvent = dataWithProbabilityPrediction[[probabilityOfChurning]]
  )
  DWP <- dataWithProbabilityPrediction # define DWP as shallow copy of (or in other words a proxy to) dataWithProbabilityPrediction
  levelNegative <- unique(DWP$label)[!unique(DWP$label) %in% levelPositive]

  setorderv(x = DWP, cols = "probabilityOfEvent", order = -1)

  # rate of the positive event in the dataWithProbabilityPrediction aka the test data set
  PRE <- prop.table(table(DWP[["label"]]))[levelPositive] # PRE is rate of the positive event in DPW data.table proxy
  message(paste("Positive event rate in your data is", scales::percent(PRE), sep = " "))
  cat("\nClass distrubution:")
  print(table(DWP$label))
  cat("\n")
  CrossTab <- function(data) base::with(data, base::table(label = label))
  depth <- function(quantile = 1) floor(quantile / 100 * nrow(DWP))
  createQuantileDT <- function(quantile = 1) { # 1 aka 1% depth of data
    # print(DWP[1:depth(quantile)])
    crossed <- CrossTab(DWP[1:depth(quantile)])
    FP <- crossed[levelNegative]
    FP <- ifelse(is.na(FP), 0, FP)
    TP <- crossed[levelPositive]
    TP <- ifelse(is.na(TP), 0, TP)
    TP_FP_Ratio <- if (FP == 0 || TP == 0) "-" else paste0("1:", round(FP / TP, 1))
    Lift <- if (TP != 0) round(prop.table(table(DWP[1:depth(quantile), label]))[levelPositive] / PRE, 3) else "-"
    return(data.table(FP = FP, TP = TP, TP_FP_Ratio = TP_FP_Ratio, Lift = Lift))
  }

  quantiles <- c(0.1, 0.5, 1:10)
  lifts <- lapply(quantiles, createQuantileDT)
  lifts <- rbindlist(lifts)
  lifts[, quantile := paste0("Q", quantiles, "%")]
  lifts[, depth := vapply(quantiles, depth, 0)]
  setcolorder(lifts, c("quantile", "depth", "FP", "TP", "TP_FP_Ratio", "Lift"))
  print(lifts)

  LiftVec <- vector(mode = "integer", length = 100L)
  for (i in 1:100) {
    p <- prop.table(table(DWP[1:depth(i), label]))[levelPositive] / PRE
    LiftVec[i] <- p
  }

  # Plot Lift curve
  gg <- ggplot(data = data.table(Quantil = 1:100, Lift = LiftVec)) +
    geom_line(aes(x = Quantil, y = Lift, color = "orange"), size = 1) +
    geom_text(aes(label = round(Lift, 2), x = Quantil, y = Lift, color = "orange"), hjust = -1, size = 2) +
    theme_minimal() +
    ggtitle("Cumulative Lift") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none")
  print(gg)

  return(lifts)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the package liftscores!")
}
