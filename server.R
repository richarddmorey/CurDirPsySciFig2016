require(lattice)
require(stats)
require(shiny)
require("RColorBrewer")
require("SVGAnnotation")
require("XML")
require(grImport)
require(shinyjs)
require("gridSVG")
library(HI)

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

  output$svg.grid2 <- renderPlot({
    #from lattice package documentation
    
    mean.prior = as.numeric(input$mean.prior)
    sd.prior = as.numeric(input$sd.prior)
    sd.lo = as.numeric(input$sd.y[1])
    sd.up = as.numeric(input$sd.y[2])
    y = as.numeric(strsplit(input$y,",",fixed=TRUE)[[1]])
    x.lo = as.numeric(input$x.lim[1])
    x.up = as.numeric(input$x.lim[2])
    
    par(mfcol=c(2,length(y)+1),mar=c(4,4,1.5,1),mgp=c(2.5,1,0),las=1,cex.lab=1.3,cex.axis=1.2,lwd=2)
    
    # estimate max y dens value
    summ = summary_posterior(mean.prior, sd.prior, y, sd.lo, sd.up)
    sd.y = sqrt(summ[["sd.posterior"]]^2 + sd.lo^2)
    y.max = dnorm(0,0,sd.y)

    
    for(i in 1:(length(y)+1)){
      y0 = rev(y[-((length(y)+1):i)])
      
      p.start = c(mean(y),.5*(sd.lo+sd.up))
      log.const = dbob_joint(p.start,mean.prior, sd.prior, y, sd.lo, sd.up, log = TRUE)
      
      samps = arms(y.start = p.start,myldens=dbob_joint,indFunc=ind.func,n.sample=500,
                   mean.prior = mean.prior, 
                   sd.prior = sd.prior, 
                   y = y0,
                   sd.lo = sd.lo, 
                   sd.up = sd.up,log.const = log.const, log = TRUE)
      
      n.points = 75
      qs = matrix(0,n.points,2)
      qs[,1] = seq(x.lo,x.up,len=n.points)
      qs[,2] = seq(sd.lo,sd.up,len=n.points)
      
      #qs = apply(samps,2,function(v) seq(min(v),max(v),len=n.points))
      dpts = expand.grid(qs[,1],qs[,2])
      
      dj = apply(dpts,1,dbob_joint,mean.prior = mean.prior, 
                 sd.prior = sd.prior, 
                 y = y0,
                 sd.lo = sd.lo, 
                 sd.up = sd.up,log.const = log.const,log=FALSE)
      dim(dj) = c(n.points,n.points)
      
      
      #filled.contour(x = qs[,1],y=qs[,2],z=dj)
      image(x = qs[,1],y=qs[,2],z=dj,ylab="TestSD",xlab="Bob's IQ",col=terrain.colors(30),main=paste("Prior for y_",i,sep=""))
      contour(x = qs[,1],y=qs[,2],z=dj,add=TRUE,drawlabels=FALSE,col=rgb(1,1,1,.3))
      if(i>1){
        points(mean(samps[,1]),mean(samps[,2]),pch=21,cex=2,col="black",bg=rgb(0,0,0,.5))
      }else{
        points(mean.prior,(sd.lo+sd.up)/2,pch=21,cex=2,col="black",bg=rgb(0,0,0,.5))
      }
      
      
      at.y = seq(20,180,len=100)
      if(i>1) 
        old.x = x
      x=rowMeans(apply(samps,1,function(pars,at){
        dnorm(at,pars[1],pars[2])
      },at=at.y))
      
      
      plot(at.y,x,xlim=c(15,135),ty='l',axes=FALSE,ylab="Density",xlab="Predicted IQ Scores",ylim=c(0,y.max))
      if(i>1)
        lines(at.y,old.x,col=rgb(0,0,0,.3),lty=2)
      axis(1)
      box()
      abline(h=0,col="gray")
      if(i<=length(y)){
        points(y[i],0,col="red",pch=19,cex=2)
      }
      if(i>1){
        points(y[0:(i-1)],rep(0,(i-1)),col="black",pch="x",cex=2)
      }
      
    }
  })

})