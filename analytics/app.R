# library(gsm.app)
dfAnalysisInput <- lAnalysisData %>%
    imap(~
        .x$Analysis_Input %>%
            mutate(MetricID = .y )
    ) %>%
    list_rbind()

run_gsm_app(
    dfAnalysisInput,
    lReportingData$Reporting_Bounds,
    lReportingData$Reporting_Groups,
    lReportingData$Reporting_Metrics,
    lReportingData$Reporting_Results,
    function() {}
)