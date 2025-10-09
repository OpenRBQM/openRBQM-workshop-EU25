#Load libraries
library(purrr)
library(dplyr)
library(gsm.core)
library(gsm.mapping)
library(gsm.kri)
library(gsm.reporting)
library(magrittr)
library(here)


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

# 6. Visualize the results with scatter plots
gsm.kri::Visualize_Scatter(
  dfFlagged,
  dfBounds = NULL,
  strGroupLabel = "GroupLevel",
  strUnit = "Visits"
)

# 7. Predict boundaries
dfBounds <- gsm.core::Analyze_NormalApprox_PredictBounds(
  dfTransformed,
  strType = "rate",
  vThreshold = c(-3, -2, 2, 3)
)

# 8. Visualize the results with predicted boundaries
gsm.kri::Visualize_Scatter(
  dfFlagged,
  dfBounds = dfBounds,
  strGroupLabel = "GroupLevel",
  strUnit = "Visits"
)

