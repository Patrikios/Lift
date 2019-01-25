#lift calculation

liftMe <- function(
  dataWithProbabilityPrediction = xgbpred2, 
  responseVariable = "Churn", 
  probabilityOfChurning = "pred"
  #cutpoint = 0.2 #cutppoint for creating a prediciton
) 
{
  
  if(!require(data.table)) { install.packages("data.table"); library(data.table)}
  if(!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2)}
  if(!require(lift)) { install.packages("lift"); library(lift)}
  
  #probability data table
  probabilityDT <- data.table(label = dataWithProbabilityPrediction[[responseVariable]], 
                              probabilityOfChurning = dataWithProbabilityPrediction[[probabilityOfChurning]])
  #probabilityDT[, predictionChurn := 0]
  #probabilityDT[probabilityOfChurning > 0.2 , predictionChurn := 1]
  #probabilityDT[, predictionChurn := as.factor(predictionChurn)]
  setorderv(x = probabilityDT, cols = "probabilityOfChurning", order = -1)
  
  #Churn rate in the testing data set
  ChurnRate <- prop.table(table(dataWithProbabilityPrediction[[responseVariable]]))['1'] #churn rate in dataWithProbabilityPrediction aka data we predict on
  print(paste("Churn rate in Test Datensatz ist", scales::percent(ChurnRate), sep = " "))
  print(table(probabilityDT$label))
  cat("\n")
  CrossTab <- function(data) with(data, table(label = label))
  depth <- function(quantile = 1) floor(quantile/100*nrow(dataWithProbabilityPrediction))
  createQuantileDT <- function(quantile = 1) { #aka 1% depth of data
    crossed <- CrossTab(probabilityDT[1:depth(quantile)])
    FP <- crossed['0']
    FP <- ifelse(is.na(FP), 0, FP)
    TP <- crossed['1']
    TP <- ifelse(is.na(TP), 0, TP)
    TP_FP_Ratio <- if(FP == 0 || TP == 0) "-" else paste0('1:', round(FP/TP)) 
    Lift <- if(TP != 0) 
      round(prop.table(table(probabilityDT[1:depth(quantile)]$label))['1']/ChurnRate, 3) 
    else ("-")
    return(data.table(FP = FP, TP = TP,  TP_FP_Ratio = TP_FP_Ratio, Lift = Lift))
  }
  
  quantiles <- c(0.1, 0.5, 1:10)
  lifts <- lapply(quantiles, createQuantileDT)
  lifts <- rbindlist(lifts)
  lifts[, quantile := paste0("Q", quantiles, "%")]
  lifts[, depth := lapply(quantiles, depth)]
  setcolorder(lifts, c("quantile", "depth", "FP", "TP", "TP_FP_Ratio", "Lift"))
  print(lifts)
  cat("\n")
  print(paste0("Top Decile: ", TopDecileLift(predicted = probabilityDT$probabilityOfChurning, labels = probabilityDT$label)))
  
  LiftVec <- vector(mode = "integer", length = 100)
  for (i in 1:100) {
    p <- prop.table(table(probabilityDT[1:depth(i)]$label))['1']/ChurnRate
    LiftVec[i] <- p
  }
  gg <- ggplot(data = data.frame(Quantil = 1:100, Lift = LiftVec)) +
    geom_line(aes(x = Quantil, y = Lift, color = 'orange'), size=1) +
    geom_text(aes(label = round(Lift, 2), x = Quantil, y = Lift, color = 'orange'), hjust = -1, size = 2) +
    theme_minimal() +
    ggtitle("Kumulatives Lift") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="none")
  print(gg)
}