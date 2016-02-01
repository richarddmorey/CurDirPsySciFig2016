# Figure 1 in Wagenmakers, Morey, and Lee

A shiny app to create (and modify) the figure in Wagenmakers, Morey, and Lee submitted to Current Directions in Psychological Science.

To run, ensure your `shiny` installation is updated (this is important) 

    install.packages(c('shiny','shinyjs','Cairo'))

you'll also need to install the `bioconductor` package `svgAnnotation`:

    source("http://bioconductor.org/biocLite.R")
    biocLite("SVGAnnotation")

and then run in R:
    
    shiny::runGitHub( "CurDirPsySciFig2016", "richarddmorey")

to run the `shiny` app. Explanation can be found on the "Instructions" tab.
