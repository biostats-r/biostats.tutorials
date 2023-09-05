#' Pre-render tutorials
#' @description Some tutorials fail to run for unknown reasons. If rendered first, they can still work.
#' @param tutorials Vector of tutorials to pre-render. If omitted will pre-render all tutorials in package
#' @param force Force pre-rendering when tutorial html files already exists. Default is to skip these for speed.
#' @importFrom learnr available_tutorials
#' @importFrom rmarkdown render
#' @importFrom purrr map %>%
#' @importFrom utils packageName
#' @importFrom glue glue
#' @importFrom fs file_exists
#
# #' @export

prerender_tutorials <- function(tutorials, force = FALSE){

  all_tutorials <-  available_tutorials(package = packageName())
  
  #check tutorials if provided
  if(!missing(tutorials)){
    tutorial_exists <- tutorials %in% all_tutorials$name
    if(!all(tutorial_exists)){
      stop(glue("Tutorial {tutorials[!tutorial_exists]} not in {packageName()} package."))
    }
  }
  #get names of tutorials if not provided
  if(missing(tutorials)){
    tutorials <- all_tutorials$name
  }
  paths <- file.path("tutorials", tutorials, tutorials)

  #remove paths with already rendered html files
  if(!force){
    paths <- paths[!file_exists(paste0(paths, ".html"))]
  }
    
  # render
  paste0(paths, ".Rmd") %>% 
    system.file(package = packageName()) %>% 
    map(render)
}
