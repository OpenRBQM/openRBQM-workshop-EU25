# TODO:
# - 
# - tweak data to introduce greater outliers
# - add explanatory comments to script and to workflows

library(purrr)
library(dplyr)
library(gsm)
library(gsm.mapping)
library(gsm.kri)
library(gsm.reporting)

# 0. Input data
# ----

lRawData <- list(
    dm = readRDS('data/SDTM/dm.rds'),
    ds = readRDS('data/SDTM/ds.rds'),
    lb = readRDS('data/SDTM/lb.rds'),
    ae = readRDS('data/SDTM/ae.rds')
)

# 1. Mapped data
# ----

lMappingWorkflows <- gsm::MakeWorkflowList(
    strPath = 'workflows/1_mappings',
    strPackage = NULL
)

lMappedData <- gsm::RunWorkflows(
    lMappingWorkflows,
    lRawData
)

# 2. Analysis Data
# ----

lAnalysisWorkflows <- gsm::MakeWorkflowList(
    strPath = 'workflows/2_metrics',
    strPackage = NULL
)

lAnalysisData <- gsm::RunWorkflows(
    lAnalysisWorkflows,
    lMappedData
)

# 3. Reporting data
# ----

lReportingWorkflows <- gsm::MakeWorkflowList(
    strPath = 'workflows/3_reporting',
    strPackage = NULL
)

lReportingData <- gsm::RunWorkflows(
    lReportingWorkflows,
    c(
        lMappedData,
        list(
            lAnalysisWorkflows = lAnalysisWorkflows,
            lAnalysisData = lAnalysisData
        )
    )
)

# 4. Output
# ----

lModuleWorkflows <- gsm::MakeWorkflowList(
    strPath = 'workflows/4_modules',
    strPackage = NULL
)

gsm::RunWorkflows(
    lModuleWorkflows,
    lReportingData
)
