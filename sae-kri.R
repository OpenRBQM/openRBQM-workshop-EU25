#Load libraries
library(purrr)
library(dplyr)
library(gsm.core)
library(gsm.mapping)
library(gsm.kri)
library(gsm.reporting)
library(magrittr)
library(here)
source('R/DataIO.R')
# Load data input/output helpers
source('R/DataIO.R')

# Calculate SAE reporting rate using gsm.core functions
dfInput <- gsm.core::Input_Rate(
  dfSubjects = gsm.core::lSource$Raw_SUBJ,
  dfNumerator = gsm.core::lSource$Raw_AE %>% filter(aeser=="Y"),
  dfDenominator = gsm.core::lSource$Raw_SUBJ,
  strSubjectCol = "subjid",
  strGroupCol = "invid",
  strNumeratorMethod = "Count",
  strDenominatorMethod = "Sum",
  strDenominatorCol = "timeonstudy"
)

# Inspect the structure of the input data frame
str(dfInput)

# 2. Transform the input for analysis
dfTransformed <- gsm.core::Transform_Rate(dfInput)

# 3. Analyze the transformed data using normal approximation
dfAnalyzed <- gsm.core::Analyze_NormalApprox(dfTransformed)

# 4. Flag sites based on thresholds
dfFlagged <- gsm.core::Flag(dfAnalyzed, vThreshold = c(-3, -2, 2, 3))

# 5. Summarize the flagged results
dfSummary <- gsm.core::Summarize(dfFlagged)

table(dfSummary$Flag) # Tabulate the flag results

# Alternative pipeline using magrittr %>%
# Uncomment to use the pipe workflow

# dfSummary_pipe <- gsm.core::Input_Rate(
#   dfSubjects = gsm.core::lSource$Raw_SUBJ,
#   dfNumerator = gsm.core::lSource$Raw_AE %>% dplyr::filter(aeser == "Y"),
#   dfDenominator = gsm.core::lSource$Raw_SUBJ,
#   strSubjectCol = "subjid",
#   strGroupCol = "invid",
#   strNumeratorMethod = "Count",
#   strDenominatorMethod = "Sum",
#   strDenominatorCol = "timeonstudy"
# ) %>%
#   gsm.core::Transform_Rate() %>%
#   gsm.core::Analyze_NormalApprox() %>%
#   gsm.core::Flag(vThreshold = c(-3, -2, 2, 3)) %>%
#   gsm.core::Summarize()
#
# table(dfSummary_pipe$Flag)
