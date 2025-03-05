SaveData <- function(lData) {
    iwalk(
        lData,
        function(dfData, strDomain) {
            strLayerName <- sub('_.*$', '', strDomain)
            strDomainName <- sub('^.*_', '', strDomain)

            saveRDS(
                dfData,
                file.path(
                    'data',
                    strLayerName,
                    paste0(strDomainName, '.rds')
                )
            )
        }
    )
}
