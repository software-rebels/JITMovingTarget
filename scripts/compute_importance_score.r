compute.importance.score <- function(fit, metrics) {
  rtn = NA

  total.chi <- anova(fit, tol=1e-10)["TOTAL","Chi-Square"]

  # Metrics that appear in the fit
  mymetrics <- metrics[metrics %in% fit$Design$name]

  my.chi=NA
  if (length(mymetrics) > 0) {
    anova.call <- paste("a=anova(fit, tol=1e-10, ", paste(mymetrics, collapse=", "), ")", sep="")
    eval(parse(text=anova.call))
    my.chi=a["TOTAL","Chi-Square"]
    my.p=a["TOTAL","P"]
  }

  rtn = c(my.chi/total.chi, my.p)
    
  return(rtn)
}
