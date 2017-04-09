source("scripts/experimental_settings.r")
source("scripts/compute_importance_score.r")

model.stability.over.time <- function(modelfits) {
  family <- c()
  value <- c()
  p <- c()
  train_period <- c()
  test_period <- c()
  fit_type <- c()

  for (trainperiod in 1:(length(modelfits)-STRATA_PER_YEAR)) {
    mtrain.local <- modelfits[[trainperiod]]$fit.local
    mtrain.all <- modelfits[[trainperiod]]$fit.all

    trainvalues.local<- list()
    trainvalues.all <- list()
    pvalues.local <- list()
    pvalues.all <- list()
    for (dimname in names(METRICS_FAMILIES)) {
      localvals <- compute.importance.score(mtrain.local,
                                            METRICS_FAMILIES[[dimname]])
      allvals <- compute.importance.score(mtrain.all,
                                          METRICS_FAMILIES[[dimname]])
      trainvalues.local[[dimname]] = localvals[1]
      pvalues.local[[dimname]] = localvals[2]
      trainvalues.all[[dimname]] = allvals[1]
      pvalues.all[[dimname]] = allvals[2]
    }

    # XXX: Show trend for 1 year
    for (testperiod in (trainperiod+1):(trainperiod+STRATA_PER_YEAR)) {
      mtest <- modelfits[[testperiod]]$fit.local
      for (dimname in names(METRICS_FAMILIES)) {
        myval = compute.importance.score(mtest, METRICS_FAMILIES[[dimname]])[1]

        value <- append(value, c(trainvalues.local[[dimname]]-myval,
                                 trainvalues.all[[dimname]]-myval))
        p <- append(p, c(pvalues.local[[dimname]], pvalues.all[[dimname]]))
        family <- append(family, rep(dimname, times=2))
        fit_type <- append(fit_type, c("Short-period", "Long-period"))
      }
        
      train_period <- append(train_period,
                             rep(paste("Train Period", trainperiod, sep=" "),
                                 times=length(names(METRICS_FAMILIES))*2))
      test_period <- append(test_period,
                            rep(testperiod,
                                times=length(names(METRICS_FAMILIES))*2))
    }
  }

  family <- factor(family, levels=c("Review", "Reviewer", "Author", "History",
                                    "Diffusion", "Size"),
                   labels=c("Review", "Rev. Exp.", "Auth. Exp.", "History",
                            "Diffusion", "Size")
                  )

  train_period <- factor(train_period,
                         levels=paste("Train Period", rep(1:length(modelfits)),
                                      sep=" ")
                        )

  test_period <- factor(test_period)
  fit_type <- factor(fit_type, levels=c("Short-period", "Long-period"))

  # XXX: Silly hack for labels in the figure
  train_period <- paste(train_period, "(i)", sep=" ")

  return(data.frame(family, train_period, test_period, fit_type, value, p))
}

do_stability_fig <- function(figure_fname, modelfits) {
  stab_data <- model.stability.over.time(modelfits)

  vals <- paste(ifelse(stab_data$value > 0, "+", ""),
                round(stab_data$value, digits=2), sep="")
  pscores <- getPLevels(stab_data$p)
  stab_data$blocklabel <- paste(vals, pscores, sep="\n")

  # Switch text colour above this threshold
  stab_data$c <- stab_data$value >= 0.2 | stab_data$value <= -0.2

  if (STRATA_PER_YEAR == 2) {
    myw <- 6
    myh <- 8
    kw <- 1.75
    ts <- 3
  } else {
    myw <- 12
    myh <- 7 
    kw <- 3.5
    ts <- 3 
  }

  ggplot(stab_data, aes(test_period, family, fill=value, label=blocklabel)) +
    facet_grid(fit_type ~ train_period, scales="free_x") +
    geom_bin2d() +
    geom_text(aes(colour=c), size = ts) + 
    scale_colour_grey(start=0, end=1, guide=F) +
    scale_fill_gradient2(name="Difference in Explanatory Power") +
    xlab("Compared Training Period (j)") +
    theme_bw(base_size=12) +
    theme(legend.position="top", legend.key.width=unit(kw, "cm"),
          axis.title.y=element_blank())

  ggsave(paste("figures", "stability", figure_fname, sep="/"),
         width=myw, height=myh)
}
