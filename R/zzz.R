#' @importFrom utils packageName
#'
.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return()
  }
  packageStartupMessage(
    "Welcome to the ", packageName(), " package.\n
Available tutorials should show up in the Tutorial tab.
Normally this is in the top right pane next
to the Environment and History tabs.\n
To see available apps, run ?biostats_apps
Alternatively, find available packages with\n
      learnr::available_tutorials(package = \"", packageName(), "\") \n
and launch them with \n
      learnr::run_tutorial(\"...\", package = \"", packageName(), "\")\n
where ... is the name of the tutorial"
  )
}
