library(purrr)
library(dplyr)
library(gsm)

# 0. Input data
# ----

# TODO:
# - add study-level metadata
# - add `InvestigatorLastName` to site-level metadata
# - add abnormal lab rate: LBNRIND == 'ABNORMAL'
# - add study discontinuation rate: DSDECOD != 'COMPLETED' where DSDECOD == 'DISPOSITION EVENT'
# - tweak data to introduce greater outliers
lRawData <- list(
    dm = readRDS('data/dm.rds'),
    ds = readRDS('data/ds.rds'),
    lb = readRDS('data/lb.rds'),
    ae = readRDS('data/ae.rds')
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
    list(
        lAnalyzed = lAnalysisData,
        lWorkflows = lAnalysisWorkflows,
        mapped_site = lMappedData$mapped_site
    )
)

# 4. Output
# ----

lModuleWorkflows <- gsm::MakeWorkflowList(
    strPath = 'workflows/4_modules',
    strPackage = NULL
)

# TODO: figure out why this is breaking
load_all('~/dev/gsm')
see <- RunWorkflows(
    lModuleWorkflows,
    lReportingData
)
Widget_ScatterPlot(
    lReportingData$Reporting_Results,
    as.list(lReportingData$Reporting_Metrics),
    lReportingData$Reporting_Groups,
    lReportingData$Reporting_Bounds
)
