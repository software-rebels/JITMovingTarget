require(rms)

################################################################################
# Experimental configuration settings
################################################################################

#
# Filter commits that touch more than FILE_THRESH files or CHURN_THRESH lines
#
FILE_THRESH <- 100
CHURN_THRESH <- 10000

#
# Select a project
#
PROJECT_NAME <- "qt"
# PROJECT_NAME <- "openstack"

#
# The number of strata in one year
#
STRATA_PER_YEAR = 4
# STRATA_PER_YEAR = 2


#
# A list containing all recognized metrics according to dimensions
#
METRICS_FAMILIES <- list(
                         Size=c("la", "ld"),
                         Diffusion=c("ns", "nd", "nf", "ent"),
                         History=c("ndev", "nuc", "age"),
                         Author=c("aexp", "arexp", "asexp", "asawr"),
                         Reviewer=c("rexp", "rrexp", "rsexp", "rsawr"),
                         Review=c("nrev", "rtime", "hcmt", "app", "self")
                        )

#
# Which function should be used to fit the models?
#
FITTOOL <- "lrm" 
# FITTOOL <- "randomForest"

#
# What parameters should we use for the fit tool?
#
FITTOOL_PARMS = list(x=T, y=T, tol=1e-10) # lrm
#FITTOOL_PARMS = list(na.action=na.omit, importance=T) # Random forest

#
# Which function should be used for fitting splines?
#
SPL = get("rcs") # Use restricted cubic splines
# SPL = function(x, y=0) { return(x) } # Turn off splines
