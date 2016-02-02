
message("If you are running this script for the first time, it may take a minute or so to install new packages.")

cran.deps = c('shiny','shinyjs','Cairo','HI')
need.packages = cran.deps[!(cran.deps %in% rownames(installed.packages()))]

if(length(need.packages)>0){
  install.packages(need.packages, dep = TRUE, quiet = TRUE)
}

if(!("SVGAnnotation" %in% rownames(installed.packages()))){
  source("http://bioconductor.org/biocLite.R")
  biocLite("SVGAnnotation", ask = FALSE)
}

shiny::runGitHub( "CurDirPsySciFig2016", "richarddmorey")
