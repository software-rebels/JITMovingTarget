source("scripts/fitness.r")

require(scales) # For muted

model.perf.over.time <- function(modelfits) {
  train_period <- c()
  test_period <- c()
  metric_type <- c()
  value <- c()
  fit_type <- c()

  for (trainperiod in 1:(length(modelfits))) {
    mlocal <- modelfits[[trainperiod]]$fit.local
    mall <- modelfits[[trainperiod]]$fit.all

    # Explanatory values
    for (m in list(mlocal, mall)) {
      value <- append(value, m$stats["Brier"])
      value <- append(value, m$stats["C"])
    }

    train_period <- append(train_period, rep(trainperiod, times=4))
    test_period <- append(test_period, rep(trainperiod, times=4))
    metric_type <- append(metric_type, c("Brier", "AUC", "Brier", "AUC"))
    fit_type <- append(fit_type, c("Short-period", "Short-period", "Long-period", "Long-period"))

    if (trainperiod == length(modelfits)) {
      next
    }

    for(testperiod in (trainperiod+1):length(modelfits)) {
      # Identify testing corpus
      testdata <- modelfits[[testperiod]]$data

      # Add Brier and AUC values
      for (m in list(mlocal, mall)) {
        value <- append(value, brier(m, testdata))
        value <- append(value, auc(m, testdata)) 
      }

      train_period <- append(train_period, rep(trainperiod, times=4))
      test_period <- append(test_period, rep(testperiod, times=4))
      metric_type <- append(metric_type, c("Brier", "AUC", "Brier", "AUC"))
      fit_type <- append(fit_type, c("Short-period", "Short-period",
                                     "Long-period", "Long-period"))
    }
  }

  train_period <- factor(train_period)
  test_period <- factor(test_period)
  metric_type <- factor(metric_type)
  fit_type <- factor(fit_type, levels=c("Short-period", "Long-period"))

  return(data.frame(train_period, test_period, metric_type, fit_type, value))
}

do_perf_fig <- function(figure_fname, modelfits) {

  jitdata <- model.perf.over.time(modelfits)

  for (met in c("AUC", "Brier")) {
    do_sub_perf_fig(subset(jitdata, metric_type == met), met,
                    gsub(".pdf", paste("_", met, ".pdf", sep=""), figure_fname),
                    modelfits
                   )
  }
}

do_sub_perf_fig <- function(myjitdata, perfmetric, figure_fname, modelfits) {
  lower = 0
  upper = NULL
  mid = NULL

  low_colour = NULL
  high_colour = NULL

  myjitdata$blocklabel <- round(myjitdata$value, digits=2)

  if (perfmetric == "Brier") {
    upper = 0.5
    mid = 0.25
    low_colour=muted("blue")
    high_colour=muted("red")
    myjitdata$c <- myjitdata$blocklabel <= 0.12
  } else if (perfmetric == "AUC") {
    upper = 1
    mid = 0.5
    low_colour=muted("red")
    high_colour=muted("blue")
    myjitdata$c <- myjitdata$blocklabel >= 0.7
  } else {
    stop(paste("Unrecognized metric: ", perfmetric, sep=""))
  }

  ggplot(myjitdata, aes(test_period, train_period, fill=value)) +
    facet_grid(. ~ fit_type) +
    scale_fill_gradient2(name=paste(perfmetric, "Score  ", sep=" "),
                         low=low_colour, high=high_colour,
                         limits=c(lower, upper), midpoint=mid) +
    geom_bin2d() +
    geom_text(aes(label = blocklabel, colour = c), size = 4.25) + 
    scale_colour_grey(start=0, end=1, guide=F) +
    xlab("Testing Period") +
    ylab("Training Period") + 
    theme_bw(base_size=22) +
    theme(legend.position="top", legend.key.width=unit(2.5, "cm"))

  ggsave(paste("figures", "perf", figure_fname, sep="/"), width=10, height=5)

  maxtrain <- max(as.numeric(myjitdata$train_period))
  train <- c()
  test <- c()
  delta <- c()
  fit_type <- c()

  for (i in 1:(maxtrain-1)) {
    traindata <- subset(myjitdata, train_period == i)
    idx <- ifelse(perfmetric == "AUC", "C", perfmetric)
    localexpl <- modelfits[[i]]$fit.local$stats[idx]
    allexpl <- modelfits[[i]]$fit.all$stats[idx]
    for (j in (i+1):maxtrain) {
      y2 <- subset(traindata, test_period == j)
      delta <- append(delta, c(y2$value[1] - localexpl, y2$value[2] - allexpl))
      test <- append(test, rep(j, times=nrow(y2)))
      train <- append(train, rep(i, times=nrow(y2)))
      fit_type <- append(fit_type, c("Short-period", "Long-period"))
    }
  }

  test <- factor(test)
  train <- factor(train)
  fit_type <- factor(fit_type, levels=c("Short-period", "Long-period"))
  deltaframe <- data.frame(train, test, delta, fit_type)
  deltaframe$blocklabel <- round(deltaframe$delta, digits=2)

  if (perfmetric == "Brier") {
    deltaframe$c <- deltaframe$blocklabel >= 0.2
  } else if (perfmetric == "AUC") {
    deltaframe$c <- deltaframe$blocklabel <= -0.2
  }

  ggplot(deltaframe, aes(test, train, fill=delta)) +
    facet_grid(. ~ fit_type) +
    scale_fill_gradient2(name=paste(perfmetric, "Score  ", sep=" "),
                         low=low_colour, high=high_colour) +
    geom_bin2d() +
    geom_text(aes(label = blocklabel, colour = c), size = 4.25) + 
    scale_colour_grey(start=0, end=1, guide=F) +
    xlab("Testing Period") +
    ylab("Training Period") + 
    theme_bw(base_size=22) +
    theme(legend.position="top", legend.key.width=unit(2.5, "cm"))

  delta_fname <- gsub(".pdf", "_delta.pdf", figure_fname)
  ggsave(paste("figures", "perf", delta_fname, sep="/"), width=10, height=5)
}
