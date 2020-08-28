## xie3xie3/ Gabi E. in GitHub, 28/08/2020
## Script that generates n means of a normally distributed sample of size N,
## then plots the result as the distribution of the mean, the collected
## means and the KDE for these.

## maincolor changes the color of the normal distribution line.
## seccolor changes the color of the kernel density estimator.
## kern is a placeholder for the kernel to be used (gaussian, epanechnikov, etc.)
## alpha is the type I error admitted for the confidence interval of
## the mean. default is 95% (alpha = 0.05) confidence. this is always
## plotted as red ablines enclosing the interval.

mean.distribution <- function(n, N, u = 0, s = 1, 
                              maincolor = rgb(54/255, 105/255, 186/255),
                              seccolor = rgb(0.2, 0.62, 0.66, 0.4),
                              kern = "g",
                              alpha = 0.05) {
  
  ## create a vector of means for n replicates
  ## of a random normally distributed sample
  sample <- colMeans(replicate(n, rnorm(N, u, s)))
  
  ## generate normal distribution across a span defined by the std error.
  error <- s/sqrt(N)
  span <- seq(u - 3*error, u + 3*error, 0.01*error)
  distribution <- dnorm(span, u, error)
  
  ## plot the distribution
  plot(span, distribution,
        type = "l", col = maincolor, lwd = 2,
        ylim = c(0, max(distribution)*6/5),
        xlim = c(u - 3*error, u + 3*error),
        main = paste("Mean of a random sample of size N =",N,"replicated",n, "times"),
        ylab = "Densidad",
        xlab = paste("Mean of X ~ N(", u, ", ", s^2,")", sep = "")
  )
    
    ## points for the replicate means
    points(x = sample, y = dnorm(sample, u, error)*4/5,
           col = rgb(0,0,0,.2))
    
    ## kernel density estimator
    lines(density(sample, kernel = kern),
          col = seccolor, lwd = 2)
    
    ## confidence interval
    abline(v = qnorm(c(alpha/2, 1 - alpha/2), u, error),
           lwd = 3, col = rgb(1, 0, 0, .4))
  }
  
}