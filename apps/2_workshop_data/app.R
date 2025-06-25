# Launch the ShinyApp
library(gsm.app)
library(fs)
library(purrr)
library(dplyr)
library(here)
source(here::here("R/DataIO.R"))

# (re)Load Workshop Data ----
dfAnalysisInput <- ReadAnalysisData()
dfBounds <- ReadData("Reporting", "Bounds")
dfGroups <- ReadData("Reporting", "Groups")
dfMetrics <- ReadData("Reporting", "Metrics")
dfResults <- ReadData("Reporting", "Results")

# Tell the app how to fetch domain data ----
fetchData <- function(strDomain,
                      strGroupID = NULL,
                      strGroupLevel = "Site",
                      strSubjectID = NULL,
                      dSnapshotDate = NULL) {
  # Load data.
  dfDomain <- ReadData("Mapped", strDomain)

  dfDomain <- dfDomain %>%
    dplyr::rename(
      "SubjectID" = "USUBJID"
    )

  if (!is.null(strGroupID) && "GroupID" %in% colnames(dfDomain)) {
    dfDomain <- dplyr::filter(dfDomain, .data$GroupID == strGroupID)
  }

  # Subset on subject ID, if given.
  if (!is.null(strSubjectID) && "SubjectID" %in% colnames(dfDomain)) {
    dfDomain <- dplyr::filter(dfDomain, .data$SubjectID == strSubjectID)
  }

  return(dfDomain)
}

run_gsm_app(
  dfAnalysisInput,
  dfBounds,
  dfGroups,
  dfMetrics,
  dfResults,
  fetchData,
  chrDomains = c(
    AE = "Adverse Events",
    LB = "Lab",
    DM = "Subject Metadata",
    DS = "Study Completion"
  ),
  strTitle = "Workshop Data",
  strFaviconColor = ColorScheme("green")
)
