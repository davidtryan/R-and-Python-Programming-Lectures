## Example taken from:

## Monte Carlo Sampling with R
# http://www.r-bloggers.com/sampling-for-monte-carlo-simulations-with-r/

##############################################################
##############################################################

# Generate a Monte Carlo sample
generateMCSample <- function(n, vals) {
  # Packages to generate quasi-random sequences
  # and rearrange the data
  require(randtoolbox)
  require(plyr)
  
  # Generate a Sobol' sequence
  sob <- sobol(n, length(vals))
  
  # Fill a matrix with the values
  # inverted from uniform values to
  # distributions of choice
  samp <- matrix(rep(0,n*(length(vals)+1)), nrow=n)
  samp[,1] <- 1:n
  for (i in 1:length(vals)) {
    l <- vals[[i]]
    dist <- l$dist
    params <- l$params
    samp[,i+1] <- eval(call(paste("q",dist,sep=""),sob[,i],params[1],params[2]))
  }
  
  # Convert matrix to data frame and label
  samp <- as.data.frame(samp)
  names(samp) <- c("n",laply(vals, function(l) l$var))
  return(samp)
}

n <- 1000  # number of simulations to run

# List described the distribution of each variable
vals <- list(list(var="Uniform",
                  dist="unif",
                  params=c(0,1)),
             list(var="Normal",
                  dist="norm",
                  params=c(0,1)),
             list(var="Weibull",
                  dist="weibull",
                  params=c(2,1)))

# Generate the sample
samp <- generateMCSample(n,vals)

# Plot with ggplot2
library(ggplot2)
samp.mt <- melt(samp,id="n")
gg <- ggplot(samp.mt,aes(x=value)) +
  geom_histogram(binwidth=0.1) +
  theme_bw() +
  facet_wrap(~variable, ncol=3,scale="free")