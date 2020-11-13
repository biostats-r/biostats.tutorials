#### Copies main copy of css file into each tutorial ####
require(purrr)
css_copier <- function(css, css_dir = "inst/css"){
  
  tutorials <- list.dirs("inst/tutorials", recursive = FALSE)
  if(missing(css)) {
    css <- list.files(path = "inst/css", pattern = ".css$")[1]
  }
  css_path <- file.path(css_dir, css)
  
  #create css directories
  map(tutorials, ~fs::dir_create(file.path(.x, "css")))
  
  #copy css file
  map(tutorials, ~fs::file_copy(path = css_path, new_path = file.path(.x, "css", css), overwrite = TRUE))
}
