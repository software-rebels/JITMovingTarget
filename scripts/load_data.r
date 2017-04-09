source("scripts/experimental_settings.r")

get.start.timestamp <- function() {
  if (PROJECT_NAME == "qt") {
    start <- 1308350292
  } else if (PROJECT_NAME  == "openstack") {
    start <- 1322599384
  }

  return(start)
}

get.end.timestamp <- function() {
  if (PROJECT_NAME == "qt") {
    end <- 1395090476
  } else if (PROJECT_NAME == "openstack") {
    end <- 1393590700
  }

  return(end)
}

# Trim away commits that are too early or too late for analysis
trim.data <- function(rawdata) {
	start <- get.start.timestamp()
	end <- get.end.timestamp()

  rawdata$churn <- rawdata$la + rawdata$ld

  rawdata$buggy <- rawdata$bugcount > 0
  rawdata$fix <- rawdata$fixcount > 0
  rawdata$bugdens <- rawdata$bugcount / rawdata$churn
  rawdata$revd <- rawdata$revd == "true"
  rawdata$self <- rawdata$self == "true"

  rawdata$tcmt <- NULL

  rawdata$asawr <- as.numeric(as.character(rawdata$asawr))
  rawdata$rsawr <- as.numeric(as.character(rawdata$rsawr))
  rawdata$osawr <- as.numeric(as.character(rawdata$osawr))

  # Apply F_5 (Too much churn, too many files, no lines added)
	rawdata <- subset(rawdata, author_date >= start & author_date < end & la > 0 &
                 churn <= CHURN_THRESH & nf <= FILE_THRESH)

  clean_idxs <- c(3:length(rawdata))
  for (i in clean_idxs) {
    if (is.logical(rawdata[,i])) {
      rawdata[,i][is.na(rawdata[,i])] <- F
    } else {
      rawdata[,i][is.na(rawdata[,i])] <- 0
    }
  }

  # Set review metrics for non-reviewed changes to NA
  for (met in c("nrev", "rtime", "hcmt", "self", "app", "rexp", "rrexp", "rsexp", "oexp", "orexp", "osexp")) {
    rawdata[!rawdata$revd,met] <- NA
  }

  # Stratify
  mymin <- min(rawdata$author_date)
  strata_size <- 60 * 60 * 24 * (365.25/STRATA_PER_YEAR) # Yearly quarters
  rawdata$strata <- factor(floor((rawdata$author_date - mymin) / strata_size))

	return(rawdata)
}

# Label all of the variables for pretty plotting
label.data <- function(trimdata) {
  label(trimdata$commit_id) <- "Commit SHA"
	label(trimdata$buggy) <- "Defect-inducing"

  # Purpose
	label(trimdata$fix) <- "Defect-fixing"

  # Size
  label(trimdata$la) <- "Lines added"
  label(trimdata$ld) <- "Lines deleted"
  label(trimdata$churn) <- "Churn"

  # Diffusion
  label(trimdata$nf) <- "Number of files"
  label(trimdata$nd) <- "Number of components"
  label(trimdata$ns) <- "Number of subsystems"
  label(trimdata$ent) <- "Entropy"

  # Review
  label(trimdata$hcmt) <- "Review comments"
  label(trimdata$rtime) <- "Review timespan"
  label(trimdata$nrev) <- "Review revisions"
  label(trimdata$app) <- "Reviewers"
	label(trimdata$self) <- "Self approval"

  # History
	label(trimdata$ndev) <- "Number of past developers"
	label(trimdata$age) <- "Time since last modification"
	label(trimdata$nuc) <- "Number of past changes"

  # Experience
	label(trimdata$aexp) <- "Author experience"
	label(trimdata$arexp) <- "Relative author experience"
	label(trimdata$asexp) <- "Subsystem author experience"
	label(trimdata$rexp) <- "Reviewer experience"
	label(trimdata$rrexp) <- "Relative reviewer experience"
	label(trimdata$rsexp) <- "Subsystem reviewer experience"
  label(trimdata$oexp) <- "Overall experience"
	label(trimdata$orexp) <- "Relative overall experience"
	label(trimdata$osexp) <- "Subsystem overall experience"

  # Awareness
  label(trimdata$asawr) <- "Author awareness"
  label(trimdata$rsawr) <- "Reviewer awareness"
  label(trimdata$osawr) <- "Overall awareness"

  return(trimdata)
}

# Load some rawdata
# Proj must be one of the above listed projects
load.data <- function() {
	rawdata <- read.csv(paste("data/", PROJECT_NAME, ".csv", sep=""))

  # Preliminary clean-up
	trimdata <- trim.data(rawdata)

  jitdata <- label.data(trimdata)

	return(jitdata)
}
