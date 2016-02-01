

install.packages(c('devtools','shiny','shinyjs','Cairo'), dep = TRUE, quiet = TRUE)
source("http://bioconductor.org/biocLite.R")
biocLite("SVGAnnotation", ask = FALSE)

shiny::runGitHub( "CurDirPsySciFig2016", "richarddmorey")

