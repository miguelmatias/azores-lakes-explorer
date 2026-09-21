# Deploy this folder to shinyapps.io.
# One-time: log in at https://www.shinyapps.io → Account → Tokens, then:
#   rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
# After that:
#   Rscript deploy.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

accounts <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
if (is.null(accounts) || !nrow(accounts)) {
  stop(
    "No shinyapps.io account stored. In R run:\n",
    "  rsconnect::setAccountInfo(name = '<account>', token = '<token>', secret = '<secret>')\n",
    "Get the token from https://www.shinyapps.io/admin/#/tokens"
  )
}

rsconnect::deployApp(
  appDir = ".",
  appName = "azores-lakes-explorer",
  appTitle = "Azores Lake Database",
  launch.browser = FALSE,
  forceUpdate = TRUE
)
cat("Deployed. URL is typically https://", accounts$name[1], ".shinyapps.io/azores-lakes-explorer/\n", sep = "")
