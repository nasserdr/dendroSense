#' Run the Shiny Graphical User Interface
#'
#' This function loads and runs the shiny GUI
#'
#' @export
#' 

runGui <- function(){
  shiny::runApp('gui/DendroSenseGUI.R')
}
