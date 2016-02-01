# Figure 1 in Wagenmakers, Morey, and Lee

A shiny app to create (and modify) the figure in Wagenmakers, Morey, and Lee submitted to Current Directions in Psychological Science.

## Quick run

This code will attempt to install the required packages and run the app:

    ## You only need the first line if you haven't installed
    ## devtools before.
    install.packages('devtools')
    devtools::source_url("https://raw.githubusercontent.com/richarddmorey/CurDirPsySciFig2016/master/quickRun.R")

## Step by step

To run, ensure your `shiny` installation is updated (this is important) 

    install.packages(c('shiny','shinyjs','Cairo'), dep = TRUE)

you'll also need to install the `bioconductor` package `svgAnnotation`:

    source("http://bioconductor.org/biocLite.R")
    biocLite("SVGAnnotation")

and then run in R:
    
    shiny::runGitHub( "CurDirPsySciFig2016", "richarddmorey")

to run the `shiny` app. Explanation can be found on the "Instructions" tab.




