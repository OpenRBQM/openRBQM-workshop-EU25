# Launch the ShinyApp
library(gsm.ae)
library(gsm.app)

run_gsm_app(
  dfAnalyticsInput = sample_dfAnalyticsInput,
  dfBounds = sample_dfBounds,
  dfGroups = sample_dfGroups,
  dfMetrics = sample_dfMetrics,
  dfResults = sample_dfResults,
  fnFetchData = sample_fnFetchData,
  lPlugins = list(pluginAE()),
  strTitle = "gsm.ae demo"
)
