require(lattice)
require(stats)
require(shiny)
require("RColorBrewer")
require("SVGAnnotation")
require("XML")
require(grImport)
require(shinyjs)
require("gridSVG")

options(shiny.usecairo=FALSE)

source('utility.R')



shinyServer(function(input,output,session) {


  output$summaries <- reactive({
    
    mean.prior = as.numeric(input$mean.prior)
    sd.prior = as.numeric(input$sd.prior)
    sd.lo = as.numeric(input$sd.y[1])
    sd.up = as.numeric(input$sd.y[2])
    y = as.numeric(strsplit(input$y,",",fixed=TRUE)[[1]])
    
    summ = summary_posterior(mean.prior, sd.prior, y, sd.lo, sd.up)
    
    post.mode = summ[["post.mode"]]
    log.const = summ[["log.const"]]
    mean.posterior = summ[["mean.posterior"]]
    sd.posterior = summ[["sd.posterior"]]
    
    dens.rat = exp(
      dbob_posterior(post.mode,mean.prior, sd.prior, y, sd.lo, sd.up, log.const = log.const, log = TRUE) -  
        dbob_posterior(70,mean.prior, sd.prior, y, sd.lo, sd.up, log.const = log.const, log = TRUE) 
    )
    
    
    paste("Prior probability IQ<70: " , round(pnorm(70, mean.prior, sd.prior)*100,1),"%<BR>", 
          "Post. probability IQ<70: " , round(pbob_posterior(70, mean.prior, sd.prior, y, sd.lo, sd.up,log.const=log.const)*100,1),"%<BR>",
          "Posterior mode (square): " , round(post.mode,1),"<BR>",
          "The density at the posterior mode (square) is " , round(dens.rat,2)," times greater than the density at 70 (circle).<BR>",
          "The posterior mean is " , round(mean.posterior,2),".<BR>",
          "The posterior standard deviation is " , round(sd.posterior,2),".<BR>",
          sep="")
  })

  output$svg.grid <- renderText({
    #from lattice package documentation
    
    mean.prior = as.numeric(input$mean.prior)
    sd.prior = as.numeric(input$sd.prior)
    sd.lo = as.numeric(input$sd.y[1])
    sd.up = as.numeric(input$sd.y[2])
    y = as.numeric(strsplit(input$y,",",fixed=TRUE)[[1]])
    x.lo = as.numeric(input$x.lim[1])
    x.up = as.numeric(input$x.lim[2])
      
    doc = svgPlot( {
      
      BobsIQplot(mean.prior,sd.prior,y,sd.lo,sd.up,x.lo,x.up)
    
    }, height = 5, width = 6, pointsize = 10)  
    
    tempsvg <- tempfile(fileext=".svg")
    on.exit(unlink(tempsvg))
    saveXML(doc, tempsvg)
    svgoutput <- readLines(con = tempsvg, n=-1)
    svgoutput
  }) 



})