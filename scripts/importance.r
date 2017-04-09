source("scripts/experimental_settings.r")
source("scripts/compute_importance_score.r")

model.importance.over.time <- function(modelfits) {
  family <- c()
  value <- c()
  period <- c()
  fit_type <- c()
  p <- c()

  for (myperiod in 1:length(modelfits)) {
    for (dimname in names(METRICS_FAMILIES)) {
      local_vals <- compute.importance.score(modelfits[[myperiod]]$fit.local,
                                             METRICS_FAMILIES[[dimname]])
      all_vals <- compute.importance.score(modelfits[[myperiod]]$fit.all,
                                           METRICS_FAMILIES[[dimname]])
      value <- append(value, c(local_vals[1], all_vals[1]))
      p <- append(p, c(local_vals[2], all_vals[2]))

      fit_type <- append(fit_type, c("Short-period", "Long-period"))

      period <- append(period, rep(myperiod, times=2))
      family <- append(family, rep(dimname, times=2))
    }
  }

  family <- factor(family, levels=c("Review", "Reviewer", "Author", "History",
                                    "Diffusion", "Size"),
                   labels=c("Review", "Rev. Exp.", "Auth. Exp.", "History",
                            "Diffusion", "Size")
                  )
  period <- factor(period)
  fit_type <- factor(fit_type, levels=c("Short-period", "Long-period"))

  return(data.frame(family, period, fit_type, value, p))
}

getPLevels <- function(p) {
  return(
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*", "")
                )
        )
  )
}

do_importance_fig <- function(figure_fname, modelfits) {

  imp_data <- model.importance.over.time(modelfits)

  importance_scores <- round(imp_data$value, digits=2)
  plevels <- getPLevels(imp_data$p)
  imp_data$blocklabel <- paste(importance_scores, plevels, sep="\n")
  imp_data$c <- imp_data$value >= 0.2 # Switch text colour above this threshold

  ggplot(imp_data, aes(period, family, fill=value)) +
    facet_grid(. ~ fit_type) +
    geom_bin2d() +
    scale_fill_gradient2(name="Explanatory Power") +
    geom_text(aes(label = blocklabel , colour = c), size = 4.25) + 
    scale_colour_grey(start=0, end=1, guide=F) +
    xlab("Training Period") +
    theme_bw(base_size=22) +
    theme(legend.position="top", legend.key.width=unit(2.5, "cm"),
          axis.title.y=element_blank())

  ggsave(paste("figures", "importance", figure_fname, sep="/"), width=10, height=6)
}
