library(purrr)
library(dplyr)
library(gsm)

# 0. Input data
# ----

#lConfig = list(
#    StudyID = 'CDISCPILOT01',
#    LoadData = function(lWorkflow, lConfig, lData = NULL) {
#        readRDS(
#            file.path(
#                'data',
#                lWorkflow$Type,
#                paste0(
#                    lWorkflow$ID,
#                    '.rds'
#                )
#            )
#        )
#    },
#    SaveData = function(lWorkflow, lConfig) {
#        saveRDS(
#            lWorkflow$lResult,
#            file.path(
#                'data',
#                lWorkflow$Type,
#                paste0(
#                    lWorkflow$ID,
#                    '.rds'
#                )
#            )
#        )
#    }
#)

# TODO:
# - tweak data to introduce greater outliers
lRawData <- list(
    dm = readRDS('data/0_raw/dm.rds'),
    ds = readRDS('data/0_raw/ds.rds'),
    lb = readRDS('data/0_raw/lb.rds'),
    ae = readRDS('data/0_raw/ae.rds')
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
) %T>% iwalk(
    ~ saveRDS(
        .x,
        file.path(
            'data',
            'Mapped',
            paste0(
                .y,
                '.rds'
            )
        )
    )
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
) %T>% iwalk(
    ~ saveRDS(
        .x,
        file.path(
            'data',
            'Analysis',
            paste0(
                .y,
                '.rds'
            )
        )
    )
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
            lAnalyzed = lAnalysisData,
            lWorkflows = lAnalysisWorkflows
        )
    )
) %T>% iwalk(
    ~ saveRDS(
        .x,
        file.path(
            'data',
            'Reporting',
            paste0(
                .y,
                '.rds'
            )
        )
    )
)

# 4. Output
# ----

lModuleWorkflows <- gsm::MakeWorkflowList(
    strPath = 'workflows/4_modules',
    strPackage = NULL
)

RunWorkflows(
    lModuleWorkflows,
    lReportingData
)
