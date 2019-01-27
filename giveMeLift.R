#lift calculation for binary response

liftMe <- function(
  dataWithProbabilityPrediction = pred,
  levelPositive = "yes",
  responseVariable = "y", 
  probabilityOfChurning = "pred"
) 
{
  lapply(c("data.table", "ggplot2"), 
         function(pak) {
          if(!pak %in% .packages()) if(!suppressWarnings(suppressPackageStartupMessages(require(pak, character.only = T)))) { 
                                            install.packages(pak); library(pak,  character.only = T)} 
                        }
         )
  
  #probability data table and its proxy
  dataWithProbabilityPrediction <- data.table(label = dataWithProbabilityPrediction[[responseVariable]], 
                                              probabilityOfEvent = dataWithProbabilityPrediction[[probabilityOfChurning]]
                                              )
  DWP = dataWithProbabilityPrediction #define DWP as shallow copy of (or in other words a proxy to) dataWithProbabilityPrediction
  levelNegative <- unique(DWP$label) [! unique(DWP$label) %in% levelPositive]
  
  setorderv(x = DWP, cols = "probabilityOfEvent", order = -1)
  
  #rate of the positive event in the dataWithProbabilityPrediction aka the test data set
  PRE <- prop.table(table(DWP[["label"]]))[levelPositive] #PRE is rate of the positive event in DPW data.table proxy
  print(paste("Positive event rate in your data ist", scales::percent(PRE), sep = " "))
  print(table(DWP$label))
  cat("\n")
  CrossTab <- function(data) with(data, table(label = label))
  depth <- function(quantile = 1) floor(quantile/100*nrow(DWP))
  createQuantileDT <- function(quantile = 1) { #1 aka 1% depth of data
    crossed <- CrossTab(DWP[1:depth(quantile)])
    FP <- crossed[levelNegative]
    FP <- ifelse(is.na(FP), 0, FP)
    TP <- crossed[levelPositive]
    TP <- ifelse(is.na(TP), 0, TP)
    TP_FP_Ratio <- if(FP == 0 || TP == 0) "-" else paste0('1:', round(FP/TP, 1))
    Lift <- if(TP != 0) round(prop.table(table(DWP[1:depth(quantile), label]))[levelPositive]/PRE, 3) else "-"
    return(data.table(FP = FP, TP = TP,  TP_FP_Ratio = TP_FP_Ratio, Lift = Lift))
  }
  
  quantiles <- c(0.1, 0.5, 1:10)
  lifts <- lapply(quantiles, createQuantileDT)
  lifts <- rbindlist(lifts)
  lifts[, quantile := paste0("Q", quantiles, "%")]
  lifts[, depth := lapply(quantiles, depth)]
  setcolorder(lifts, c("quantile", "depth", "FP", "TP", "TP_FP_Ratio", "Lift"))
  print(lifts)
  
  LiftVec <- vector(mode = "integer", length = 100)
  for (i in 1:100) {
    p <- prop.table(table(DWP[1:depth(i), label]))[levelPositive]/PRE
    LiftVec[i] <- p
  }
  
  #Plot Lift curve
  gg <- ggplot(data = data.table(Quantil = 1:100, Lift = LiftVec)) +
    geom_line(aes(x = Quantil, y = Lift, color = "orange"), size = 1) +
    geom_text(aes(label = round(Lift, 2), x = Quantil, y = Lift, color = 'orange'), hjust = -1, size = 2) +
    theme_minimal() +
    ggtitle("Cumulative Lift") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position="none")
  print(gg)
}
