#' @importFrom utils packageName
#' 
.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return()
  }
  if (rstudioapi::versionInfo()$version < 1.3) {
    warning("You have an old version of RStudio which may not completely support learnr tutorial.\nPlease upgrade RStudio if possible")
  } else {
    packageStartupMessage(
      "Welcome to the", packageName(), "package.\n
Available tutorials shold show up in the Tutorial tab.
Normally this is in the top right pane next
to the Environment and History tabs\n
Alternatively, find available packages with\n
      learnr::available_tutorials(package = \"", packageName(), "\") \n
and launch them with \n
      learnr::run_tutorial(..., package = \"", packageName(), "\")\n
where ... is the name of the tutorial")
  }
}