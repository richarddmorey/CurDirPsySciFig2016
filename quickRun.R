
message("If you are running this script for the first time, it may take a minute or so to install new packages.")

if(!all(c('shiny','shinyjs','Cairo') %in% rownames(installed.packages()))){
  install.packages(c('shiny','shinyjs','Cairo','HI'), dep = TRUE, quiet = TRUE)
}

if(!("SVGAnnotation" %in% rownames(installed.packages()))){
  source("http://bioconductor.org/biocLite.R")
  biocLite("SVGAnnotation", ask = FALSE)
}

shiny::runGitHub( "CurDirPsySciFig2016", "richarddmorey")
