source("scripts/experimental_settings.r")
source("scripts/load_data.r")
source("scripts/fit_models.r")
source("scripts/performance.r")
source("scripts/importance.r")
source("scripts/stability.r")


produce.figs <- function() {
  # Names for the figures that will be generated
  figure_fname <- paste(paste(PROJECT_NAME, FITTOOL, STRATA_PER_YEAR, sep="_"),
                        "pdf", sep=".")
  
  # Load, filter, and stratify project_name data
  jitdata <- load.data() 

  # Fit a series of models to the JIT data
  ##############################################################################
  # NOTE: Feature preprocessing has already been performed on the shared datasets
  # If you do extend this work in the future, please perform some form of feature
  # reduction before fitting (if the fit function expects it)
  ##############################################################################
  modelfits <- fit.models(jitdata)

  #
  # Build performance figures
  #
  do_perf_fig(figure_fname, modelfits)

  #
  # Build importance figure
  #
  do_importance_fig(figure_fname, modelfits)

  #
  # Build stability figure
  #
  do_stability_fig(figure_fname, modelfits)
}
