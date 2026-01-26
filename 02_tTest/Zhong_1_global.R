# SETUP ENVIRONMENT ----
library(devtools)
library(plyr)
library(rio)
library(tidyverse)
library(reshape2)
library(stats)

# TODO: Set up your directory
project.root <- file.path("~", "projects", "manyLabsE")
OSFdata.root <- file.path(project.root, "OSFdata")

source(file.path(project.root, "00_utils", "WYQ_manylabRs_SOURCE.R"))

# ANALYSIS INFO ----
study.description <- "Moral Cleansing (Zhong & Liljenquist, 2006)"
analysis.unique.id <- 65
analysis.name <- "Zhong.1"
analysis.type <- 1
analysis.type.name <- "study_global_include"
analysis.type.groups <- "Source.Global"
Nmin.raw <- 30
Nmin.cond <- 15
# subset -> subset.type to avoid conflicts
subset.type <- "all"

# GET LOOKUP TABLES ----
ML2.key <- rio::import(file.path(project.root, "00_data", "ML2_KeyTable.csv"))
ML2.key <- ML2.key[!is.na(ML2.key$unique.id) & ML2.key$unique.id == analysis.unique.id, ]
SourceInfoTable <- rio::import(file.path(OSFdata.root, "!!KeyTables", "ML2_SourceInfo - ML2_SourceInfo.csv"))

# Get the correct slate according to info in ML2.key['study.slate']
if (ML2.key$study.slate == 1) {
  ML2.df <- rio::import(file.path(OSFdata.root, "!!RawData", "ML2_S1.csv"))
} else {
  ML2.df <- rio::import(file.path(OSFdata.root, "!!RawData", "ML2_S2.csv"))
}

# Add a unique ID
ML2.df$uID <- seq(1, nrow(ML2.df))

# Get info to create a dataset for the current study
# keytable <- ML2.key
ML2.in <- get.info(ML2.key, colnames(ML2.df), subset.type)

# Generate chain to select variables for the data frame and create a filter chain for the variables to use for analysis
# Info based on KeyTable information in study.vars, cases.include, site.include, params.NA
ML2.id <- get.chain(ML2.in)

# Apply the df chain to select relevant subset of variables

ML2.df <- ML2.df %>%
  dplyr::select(2, 7, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 805, 904, 905, 906, 907, 908, 909, 910, 911, 912, 913, 914, 915, 934, 935, 938, 939, 940) %>%
  dplyr::filter(is.character(source))

# Decide which analyses to run on which groups
toRun <- decide.analysis(ML2.key, analysis.unique.id, analysis.type, doAll = TRUE)

if (nrow(ML2.df) <= 0 || length(toRun$studiess) <= 0) {
  print("No tests to run, nothing selected!")
  stop()
}



# Create a variable indicating the study order for each case
ML2.df$study.order <- NA
stmp <- strsplit(ML2.df$StudyOrderN, "[|]")

# Correct differences in study names
Stud <- ML2.key$study.name

ML2.df$study.order <- plyr::laply(seq_along(stmp), function(o) {
  which(grepl(Stud, stmp[[o]])) %00% NA
})

ML2.sr <- list()
ML2.var <- list()
outputSource <- list()
dataSource <- list()
raw.df <- list()
clean.df <- list()
cleanData <- list()
testVarEqual <- ML2.in$stat.params$var.equal

# Loop over sites in runGroups within a study
if (analysis.type == 1) {
  runGroups <- "all"
} else {
  runGroups <- sort(na.exclude(unique(ML2.df[[toRun$ugroup]])))
}

disp(paste(analysis.unique.id, ML2.key$study.analysis, "- START"), header = toupper(ML2.key$study.analysis), footer = FALSE)
cat("\n")


# START GROUPS ----
g <- 1

# Include only datasets that have N >= Nmin.raw & n.group >= Nmin.cond
listIT <- FALSE
nMin1 <- FALSE
nMin2 <- FALSE
compN <- compN1 <- compN2 <- 0

gID <- rep(TRUE, nrow(ML2.df))

# Check nMin
if (sum(gID, na.rm = TRUE) >= Nmin.raw) {
  nMin1 <- TRUE
  # Get a list containing the data frames to be used in the analysis
  ML2.sr[[g]] <- get.sourceData(ML2.id, ML2.df[gID, ], ML2.in)
}

# Double-check nMin
if (nMin1) {
  compN <- ML2.sr[[g]]$N
  compN1 <- sum(ML2.sr[[g]]$RawDataFilter[[1]]$Included, na.rm = TRUE)
  compN2 <- sum(ML2.sr[[g]]$RawDataFilter[[2]]$Included, na.rm = TRUE)
  if (any(compN >= Nmin.raw) & (all(compN1 >= Nmin.cond, compN2 >= Nmin.cond))) {
    nMin2 <- TRUE
  }
}

# START ANALYSIS ----------------------------------------

if (all(nMin1, nMin2)) {
  # To see the function code type:varfun.Zhong.1, or lookup in manylabRs_SOURCE.R
  ML2.var[[g]] <- varfun.Zhong.1(ML2.sr[[g]])


  # Check equal variance assumption
  if (!is.na(testVarEqual)) {
    if (testVarEqual) {
      logtxt <- paste(analysis.unique.id, ML2.key$study.analysis, "-", runGroups[g])
      ML2.in$stat.params$var.equal <- decide.EqualVar(ML2.var[[g]], ML2.in$study.vars.labels, ML2.key, group = logtxt) # don't pass the cleanData frame
    }
  }

  # Run the analysis according to ML2.key: 'stat.test'
  stat.params <<- ML2.in$stat.params


  stat.test <- try.CATCH(with(ML2.var[[g]], t.test(x = Ethical, y = Unethical, conf.level = stat.params$conf.level, var.equal = stat.params$var.equal, alternative = stat.params$alternative)))
}

# Alexander -----

sourceColumn <- character(length = dim(ML2.var[[1]]$cleanDataFilter)[1])

for (i in seq_along(sourceColumn)) {
  iets <- ML2.df[ML2.df$uID == ML2.var[[1]]$cleanDataFilter$uID[i], ]
  sourceColumn[i] <- iets$source
}

ML2.var[[1]]$cleanDataFilter$source <- sourceColumn

zhongData <- ML2.var[[1]]

# Alexander: The following is Zhong data specific
#
dat <- zhongData$cleanDataFilter
kep <- unique(dat$source)
#
# dat$source[24] == "bogota" the factor only has one level, thus, we remove it
allSources <- kep[-24]

vep <- dat[dat$source %in% allSources, ]

t.test(variable ~ factor, data = vep, var.equal = TRUE)


# save(zhongData, file="zhongData.RData")


# Analysis per source ----
dat <- zhongData$cleanDataFilter

allSources <- unique(dat$source)

# NOTE(Alexander): Unfortunately, it's unclear to me what happened to
# bogota nCopied.zhon1
#
# See: ML2.df[ML2.df$source=="bogota", ]$nCopied.zhon1
allSources <- allSources[-24]

eValues <- pValues <- numeric(length(allSources))

designObj <- designSafeT(nPlan = c(40, 40), testType = "twoSample")

for (i in seq_along(eValues)) {
  someData <- dat[dat$source == allSources[i], ]

  x <- someData$variable[someData$factor == "Ethical"]
  y <- someData$variable[someData$factor == "Unethical"]

  bob <- t.test(x, y, var.equal = TRUE, data = someData)
  pValues[i] <- bob$p.value

  bob <- safeTTest(x, y, designObj = designObj)
  eValues[i] <- bob$eValue
}

pValues < 0.05
eValues > 20


i <- 1

pValues <- matrix(nrow = length(allSources), ncol = 363)

sigPValue <- numeric(length(allSources))

for (i in seq_along(sigPValue)) {
  someData <- dat[dat$source == allSources[i], ]

  x <- someData$variable[someData$factor == "Ethical"]
  y <- someData$variable[someData$factor == "Unethical"]

  n1 <- length(x)
  n2 <- length(y)

  ratio <- n2 / n1

  for (j in 2:n1) {
    tempRes <- t.test(x[1:j], y[1:ceiling(j * ratio)], var.equal = TRUE)
    pValue <- tempRes$p.value
    pValues[i, j] <- pValue

    if (pValue < 0.05 && sigPValue[i] != 1) {
      sigPValue[i] <- 1
    }
  }
}

mean(sigPValue)


bep <- ML2.df[ML2.df$source == "bogota", ]

bep$nCopied.zhon2

someColumnNames <- c(
  "zhon.dv.1_1", "zhon.dv.1_2", "zhon.dv.1_3",
  "zhon.dv.1_4", "zhon.dv.1_5", "zhon.dv.1_6",
  "zhon.dv.1_7", "zhon.dv.1_8", "zhon.dv.1_9",
  "zhon.dv.1_10"
)

kaas <- bep[someColumnNames]
sum(kaas[1, ])

rowsum.data.frame(bep[someColumnNames])
