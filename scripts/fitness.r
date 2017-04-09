require(zoo) # For rollmean

################################################################################
# Brier score
################################################################################
brier <- function(fit, testdata) {
  probs <- pred.probs(fit, testdata)
  resids <- ifelse(testdata$buggy == T, 1, 0)-probs
  return(sum(resids^2)/nrow(testdata))
}

pred.probs <- function(fit, testdata) {
  odds <- predict(fit, testdata)

  if (class(fit)[2] == "randomForest") {
    odds <- predict(fit, testdata, type="prob")[,"TRUE"]
  }

  probs <- exp(odds)/(1+exp(odds))
  probs[is.na(probs)] <- 1 # XXX: NAs are treated as buggy

  return(probs)
}

################################################################################
# AUC
################################################################################

auc <- function(fit, testdata) {
  probs <- pred.probs(fit, testdata)

  tpr <- c()
  fpr <- c()
  for (thresh in seq(0,1,by=0.01)) {
    preds <- probs >= thresh

    buggy_preds <- preds[testdata$buggy]
    clean_preds <- preds[!testdata$buggy]

    tp <- sum(ifelse(buggy_preds == T, 1, 0))
    fp <- sum(ifelse(clean_preds == T, 1, 0))
    tpr <- append(tpr, tp / length(buggy_preds))
    fpr <- append(fpr, fp / length(clean_preds))
  }

  #plot(fpr, tpr, xlim=c(0,1), ylim=c(0,1))

  return(-areauc(fpr, tpr))
}

areauc <- function(x, y) {
  return(sum(diff(x)*rollmean(y,2)))
}
