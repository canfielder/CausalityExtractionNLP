#' The following script is used for ensuring packages are installed
#' across the entire project. This script provides a single location
#' for documenting which packages are used. Any individual script or
#' notebook only needs to load packages, not install.
#

project_install_packages <- function(){
  if (!require(pacman)) {install.packages('pacman')}
  p_load(
    caTools,
    dplyr,
    pdftools,
    quanteda,
    readxl,
    reticulate,
    rJava,
    stringr,
    tabulizer,
    textstem,
    tidyr,
    tidytext,
    tokenizers
  )
}
