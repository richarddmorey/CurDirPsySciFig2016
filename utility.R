#################################################################
##              function that creates                          ##
##              Bob's IQ prior and posterior plot              ##
##              function written by Quentin F. Gronau          ##
##              adapted somewhat by Richard D. Morey           ##
#################################################################


BobsIQplot <- function(mean.prior, sd.prior, y, sd.lo, sd.up, x.lo=40, x.up=115) {
 
  
  summ = summary_posterior(mean.prior, sd.prior, y, sd.lo, sd.up)
  
  post.mode = summ[["post.mode"]]
  log.const = summ[["log.const"]]
  mean.posterior = summ[["mean.posterior"]]
  sd.posterior = summ[["sd.posterior"]]
  
  ### plot settings
  xlim = c(x.lo, x.up)
  #xlim = range(c(mean.prior+c(-3,3)*sd.prior,mean.posterior+c(-3,3)*sd.posterior))
  ylim <- c(0, 1.2*dbob_posterior(post.mode, mean.prior, sd.prior, y, sd.lo, sd.up, log = FALSE, log.const = log.const))
  lwd <- 2
  lwd.points <- 2
  lwd.axis <- 1.2
  cex.points <- 1.4
  cex.axis <- 1.2
  cex.text <- 1.1
  cex.labels <- 1.3
  cexLegend <- 1.2
  
  op <- par(mar = c(5.1, 4.1, 4.1, 2.1))
  
  ### create empty canvas
  plot(1, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "")
  
  ### shade prior area < 70
  greycol1 <- rgb(0, 0, 0, alpha = 0.2)
  greycol2 <- rgb(0, 0, 0, alpha = 0.4)
  
  polPrior <- seq(xlim[1], 70, length.out = 400)
  xx <- c(polPrior, polPrior[length(polPrior)], polPrior[1])
  yy <- c(dnorm(polPrior, mean.prior, sd.prior), 0, 0)
  polygon(xx, yy, col = greycol1, border = NA)
  
  ### shade posterior area < 70
  polPosterior <- seq(xlim[1], 70, length.out = 400)
  xx <- c(polPosterior, polPosterior[length(polPosterior)], polPosterior[1])
  yy <- c(dbob_posterior(polPosterior, mean.prior, sd.prior, y, sd.lo, sd.up, log = FALSE, log.const = log.const), 0, 0)
  #yy <- c(dnorm(polPosterior, mean.posterior, sd.posterior), 0, 0)
  polygon(xx, yy, col = greycol2, border = NA)
  
  ### grey dashed lines to prior mean, posterior mean and posterior at 77
  lines(rep(mean.prior, 2), c(0, dnorm(mean.prior, mean.prior, sd.prior)), lty = 2, col = "grey", 
        lwd = lwd)
  lines(rep(mean.posterior, 2), c(0, dbob_posterior(mean.posterior, mean.prior, sd.prior, y, sd.lo, sd.up, log = FALSE, log.const = log.const)), 
        lty = 2, col = "grey", lwd = lwd)
  
  ### axes
  axis(1, at = seq(ceiling(xlim[1]/5)*5, floor(xlim[2]/5)*5, 5), cex.axis = cex.axis, lwd = lwd.axis)
  axis(2, labels = FALSE, tck = 0, lwd = lwd.axis, line = -0.5)
  
  ### axes labels
  mtext("Bob's IQ", side = 1, cex = 1.6, line = 2.4)
  mtext("Density", side = 2, cex = 1.5, line = 0)
  
  ### plot prior and posterior
  
  xx = sort(c(seq(x.lo,x.up,len=50),qnorm(1:30/31,mean.prior,sd.prior),qnorm(1:30/31,mean.posterior,sd.posterior)))

  
  # prior
  lines(xx,dnorm(xx, mean.prior, sd.prior), lwd = lwd, lty = 3)
  
  # posterior
  lines(xx,dbob_posterior(xx, mean.prior, sd.prior, y, sd.lo, sd.up, log = FALSE, log.const = log.const), 
       lwd = lwd)
  
  ### add points
  
  # posterior density at 70
  points(70, dbob_posterior(70, mean.prior, sd.prior, y, sd.lo, sd.up, log = FALSE, log.const = log.const), pch = 21, bg = "white", cex = cex.points, 
         lwd = lwd.points)
  
  # maximum a posteriori value
  points(post.mode, dbob_posterior(post.mode, mean.prior, sd.prior, y, sd.lo, sd.up, log = FALSE, log.const = log.const), pch = 22, 
         bg = "white", cex = cex.points, lwd = lwd.points)
  
  ### credible interval
  CI0 = qbob_posterior(c(.025,.975),mean.prior, sd.prior, y, sd.lo, sd.up, log.const = log.const)
  
  CIlow <- CI0[1]
  CIhigh <- CI0[2]
  yCI <- ylim[2]*.9
  
  arrows(CIlow, yCI, CIhigh, yCI, angle = 90, code = 3, length = 0.1, lwd = lwd)
  text(mean.posterior, yCI + 0.0042, labels = "95%", cex = cex.text)
  text(CIlow, yCI, labels = paste(round(CIlow, 2)), cex = cex.text, pos = 2, offset = 0.3)
  text(CIhigh, yCI, labels = paste(round(CIhigh, 2)), cex = cex.text, pos = 4, offset = 0.3)
  
  ### legend
  legend("topright", legend = c("Posterior", "Prior"), lty = c(1, 3),
         bty = "n", lwd = c(lwd, lwd), cex = cexLegend, xjust = 1, yjust = 1, x.intersp = 0.6, 
         seg.len = 1.2)

  par(op)
  
}

### integrate() functions for posterior
### All functions below by Richard D. Morey

dbob_posterior <- Vectorize(function(mu, mean.prior, sd.prior, y, sd.lo, sd.up, log = FALSE, shift = 0, moment = 0, log.const = 0){
  mu = mu - shift
  N = length(y)
  alpha = (N + 1)/2
  beta = sum( (y - mu)^2 ) / 2
  int.sig  = pgamma(1/sd.lo^2, shape = alpha, rate = beta) - pgamma(1/sd.up^2, shape = alpha, rate = beta)
  if( log & moment) stop("log and moment cannot both be TRUE.")
  la = log(int.sig) + -N/2*log(2*pi) - log(2*(sd.up - sd.lo)) + lgamma(alpha) - alpha*log(beta) + dnorm(mu, mean.prior, sd.prior, log = TRUE) - log.const
  if(log){
    return(la)
  }else{
    return(mu^moment*exp(la))
  }
},"mu")

pbob_posterior <- Vectorize(function(mu, mean.prior, sd.prior, y, sd.lo, sd.up, log.const = 0){
  integrate(dbob_posterior,-Inf,mu,
            mean.prior = mean.prior, 
            sd.prior = sd.prior, 
            y = y,
            sd.lo = sd.lo, 
            sd.up = sd.up,log.const = log.const)[[1]]
},"mu")

qbob_posterior <- Vectorize(function(p, mean.prior, sd.prior, y, sd.lo, sd.up, log.const = 0 ){
  summ = summary_posterior(mean.prior, sd.prior, y, sd.lo, sd.up)
  post.mode = summ[["post.mode"]]
  sd.posterior = summ[["sd.posterior"]]
  m0 = qnorm(plogis(qlogis(p)+c(-1,1)),post.mode,sd.posterior)
  optimize(function(x,...){
    (qlogis(pbob_posterior(x,...)) - qlogis(p))^2   
  },m0,
  mean.prior = mean.prior, 
  sd.prior = sd.prior, 
  y = y,
  sd.lo = sd.lo, 
  sd.up = sd.up,
  log.const = log.const)$minimum  
},"p")


bob_post_mode = function(mean.prior, sd.prior, y, sd.lo, sd.up){
  N = length(y)
  lims = c(0,200)
  optimize(dbob_posterior,lims,
           mean.prior = mean.prior, 
           sd.prior = sd.prior, 
           y = y,
           sd.lo = sd.lo, 
           sd.up = sd.up,
           log = TRUE, maximum = TRUE)$maximum
}

var.func.bob = function(mu,ex.mu,mean.prior, sd.prior, y, sd.lo, sd.up, log.const){
  exp(2*log(abs(mu - ex.mu)) + dbob_posterior(mu,mean.prior, sd.prior, y, sd.lo, sd.up, log.const = log.const, log = TRUE))
}


summary_posterior <- function(mean.prior, sd.prior, y, sd.lo, sd.up){
  
  post.mode = bob_post_mode(mean.prior, sd.prior, y, sd.lo, sd.up)
  log.const = log(integrate(dbob_posterior,-Inf,Inf,
                            mean.prior = mean.prior, 
                            sd.prior = sd.prior, 
                            y = y,
                            sd.lo = sd.lo, 
                            sd.up = sd.up, shift = -post.mode)[[1]])
  
  mean.posterior = integrate(dbob_posterior,-Inf,Inf,
                             mean.prior = mean.prior, 
                             sd.prior = sd.prior, 
                             y = y,
                             sd.lo = sd.lo, 
                             sd.up = sd.up, moment = 1, log.const = log.const,shift = -post.mode)[[1]]
  
  sd.posterior = sqrt(integrate(var.func.bob,-Inf,Inf,
                                ex.mu = mean.posterior,
                                mean.prior = mean.prior, 
                                sd.prior = sd.prior, 
                                y = y,
                                sd.lo = sd.lo, 
                                sd.up = sd.up,log.const = log.const)[[1]])
  
  return(c(
    post.mode = post.mode,
    log.const = log.const,
    mean.posterior = mean.posterior,
    sd.posterior = sd.posterior
  ))
}


dbob_joint <- function(pars, mean.prior, sd.prior, y, sd.lo, sd.up, log.const = 0, log = FALSE){
  la =  dnorm( pars[1], mean.prior, sd.prior, log = TRUE) +
    log(pars[2]>sd.lo & pars[2]<sd.up) - log.const
  if(length(y)>0){
    la = la + sum(dnorm(y, pars[1], pars[2], log = TRUE))
  }
  if(log){
    return(la)
  }else{
    return(exp(la))
  }
}

ind.func = function(pars, mean.prior, sd.prior, y, sd.lo, sd.up,...){
  pars[1]>20 & pars[1]<180 & pars[2]>sd.lo & pars[2]<sd.up
}

