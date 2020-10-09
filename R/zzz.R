#' 
.onAttach <- function(libname, pkgname){
  if(!interactive()){
    return()
  }
  if(rstudioapi::versionInfo()$version < 1.3){
    warning("You have an old version of Rstudio which may not completely support learnr tutorial.\nPlease update if possible")
  } else {
    packageStartupMessage("Available tutorials shold show up in the Tutorial tab.\nNormally this is in the top right pane next to the Environment and History tabs\nAlternatively, find available packages with\nlearnr::available_tutorials(package = \"", packageName(), "\") \nand launch them with \nlearnr::run_tutorial(..., package = \"", packageName(), "\")\nwhere ... is the name of the tutorial")
    
  }
}