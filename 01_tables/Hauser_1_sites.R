# SETUP ENVIRONMENT ----
library(devtools)
library(plyr)
library(rio)
library(tidyverse)
library(reshape2)
library(exact2x2)

# TODO: Set up your directory
project.root <- file.path("~", "projects", "manyLabsE")
OSFdata.root <- file.path(project.root, "OSFdata")

# ANALYSIS INFO ----
study.description <- "Trolley Dilemma 1 (Hauser et al., 2007)"
analysis.unique.id <- 39
analysis.name <- "Hauser.1"
analysis.type <- 3
analysis.type.name <- "study_secondary_include"
analysis.type.groups <- "Source.Secondary"
Nmin.raw <- 30
Nmin.cond <- 15
# subset -> subset.type to avoid conflicts
subset.type <- "sites"

source(file.path(project.root, "00_utils", "WYQ_manylabRs_SOURCE.R"))


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

# PREPARE DATA & OUTPUT ----

# Add a unique ID
ML2.df$uID <- seq(1, nrow(ML2.df))

# Get info to create a dataset for the current study
ML2.in <- get.info(ML2.key, colnames(ML2.df), subset.type)

# Generate chain to select variables for the data frame and create a filter chain for the variables to use for analysis
# Info based on KeyTable information in study.vars, cases.include, site.include, params.NA
ML2.id <- get.chain(ML2.in)

# Apply the df chain to select relevant subset of variables

ML2.df <- ML2.df %>%
  dplyr::select(2, 7, 181, 182, 191, 192, 521, 522, 523, 524, 525, 526, 527, 528, 529, 530, 531, 532, 535, 536, 537) %>%
  dplyr::filter(is.character(source) & (Weird == 0))


# Decide which analyses to run on which groups
toRun <- decide.analysis(ML2.key, analysis.unique.id, analysis.type, doAll = TRUE)

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
runGroups <- sort(na.exclude(unique(ML2.df[[toRun$ugroup]])))
print(paste0(length(runGroups), " sites in total"))

# START GROUPS ----
# for (g in seq_along(runGroups)) {
for (g in 1:2) {

  # Include only datasets that have N >= Nmin.raw & n.group >= Nmin.cond
  listIT <- FALSE
  nMin1 <- FALSE
  nMin2 <- FALSE
  compN <- compN1 <- compN2 <- 0

  gID <- ML2.df$source %in% runGroups[g]

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
    # To see the function code type:varfun.Hauser.1, or lookup in manylabRs_SOURCE.R
    ML2.var[[g]] <- varfun.Hauser.1(ML2.sr[[g]])


    # Check equal variance assumption
    if (!is.na(testVarEqual)) {
      if (testVarEqual) {
        logtxt <- paste(analysis.unique.id, ML2.key$study.analysis, "-", runGroups[g])
        ML2.in$stat.params$var.equal <- decide.EqualVar(ML2.var[[g]], ML2.in$study.vars.labels, ML2.key, group = logtxt) # don't pass the cleanData frame
      }
    }

    # Run the analysis according to ML2.key: 'stat.test'
    stat.params <<- ML2.in$stat.params

    table <- with(ML2.var[[g]], table(Condition, Response))
    print(paste0("Running test for ", runGroups[[g]]))
    print(table)
    print(fisher.exact(table)$p.value)
    rm(table)
  }

}
