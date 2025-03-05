# library(gsm.app)
# library(gsm.ae)
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
    function(
        strDomain,
        strSiteID = NULL,
        strSubjectID = NULL
    ) {
        strDomain <- switch(strDomain,
            SUBJ = 'DM',
            STUDCOMP = 'DS',
            strDomain
        )
        # Load data.
        dfDomain <- file.path('data', 'Mapped', paste0(strDomain, '.rds')) %>%
            readRDS() %>%
            mutate(
                SubjectID = USUBJID
            )

        # Subset on site ID, if available.
        if (!is.null(strSiteID)) {
            dfDomain <- dfDomain %>%
                dplyr::filter(
                    .data$SITEID == strSiteID
                )
        }

        # Subset on subject ID, if available.
        if (!is.null(strSubjectID)) {
            dfDomain <- dfDomain %>%
                dplyr::filter(
                    .data$USUBJID == strSubjectID
                )
        }

        return(dfDomain)
    },
    # TODO: figure out how to add new domains
    chrDomains = c(
        'SUBJ',
        'STUDCOMP',
        'LB',
        'AE'
    ),
    lPlugins = list(pluginAE())
)
