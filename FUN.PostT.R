# Load necessary library
library(mvtnorm)

# Define sample size for each group and the number of groups
n = nX = nY = nZ = nT = 100  # Sample size for each group (X, Y, Z, T)

# Define the variance (standard deviation squared) for each group
sigX2x = sigY2x = sigZ2x = sigT2x = 12^2  # Variance for each group

# Define the sample means for each group
ryX = 82; ryY = 88; ryZ = 88; ryT = 90  # Sample means for each group

# Generate random data for each group
tdata = rbind(data.frame(y = rnorm(n, ryX, sqrt(sigX2x)), trt = "X"),
              data.frame(y = rnorm(n, ryY, sqrt(sigY2x)), trt = "Y"),
              data.frame(y = rnorm(n, ryZ, sqrt(sigZ2x)), trt = "Z"),
              data.frame(y = rnorm(n, ryT, sqrt(sigT2x)), trt = "T"))

# Define prior mean and variance
mu0 = 86 
   
# Now we use generic code to do the calculation assuming variance unknown
postP.Tfun = function(tdata, premu, presize, p3, p4) {
  # Get unique treatment groups and their counts
  ntrt <- sort(unique(tdata$trt))
  nntrt <- length(ntrt)
  
  # If prior parameters are scalars, repeat them for all groups
  if (length(premu) == 1) premu <- rep(premu, nntrt)
  if (length(presize) == 1) presize <- rep(presize, nntrt)
  if (length(p3) == 1) p3 <- rep(p3, nntrt)
  if (length(p4) == 1) p4 <- rep(p4, nntrt)
  
  # Compute posterior means and standard deviations for each group
  df = post_means = post_scale = ybar = numeric(nntrt) 
  sntrt = sort(ntrt)
  
  for (i in 1:nntrt) {
    group_data <- tdata$y[tdata$trt == sntrt[i]]
    n_i <- length(group_data)
    ybar[i] = ybar_i <- mean(group_data)
    sig2_i <- var(group_data)
    
    # Degrees of freedom for t-distribution
    df[i] = 2 * (p3[i] + n_i / 2)
    
    # Posterior mean (location parameter)
    post_means[i] = (premu[i] * presize[i] + n_i * ybar_i) / (presize[i] + n_i)
    
    # Posterior scale parameter (accounts for sample variance and prior contribution)
    post_scale[i] = sqrt(((1 / 2) * (n_i - 1) * sig2_i + (presize[i] * n_i * (ybar_i - premu[i])^2) / 
                            (2 * (presize[i] + n_i))) / ((p3[i] + n_i / 2) * (presize[i] + n_i)))
  }
  
  # Loop to compute posterior probability for each group
  for (j in 1:length(sntrt)) {
    # Identify the target group (T in your case)
    target_idx <- which(ntrt == sntrt[j])  # Change if needed
    target_mean <- post_means[target_idx]
    target_scale <- post_scale[target_idx]
    
    # Compute the probability that T has the highest mean
    integrand <- function(x) {
      prob <- 1
      for (i in 1:nntrt) {
        if (i != target_idx) {
          a <- (target_mean - post_means[i]) / post_scale[i]
          b <- target_scale / post_scale[i]
          prob <- prob * pt(a + b * x, df[i])
        }
      }
      prob * dt(x, df[target_idx])  # Multiply by standard t-distribution density
    }
    
    # Integrate over all possible x (standard normal)
    Pi <- integrate(integrand, lower = -Inf, upper = Inf)$value
    if (j == 1) {
      outp = c(sntrt[j], Pi)
    } else {
      outp = rbind(outp, c(sntrt[j], Pi))
    }
  }
  
  doutp = data.frame(outp, post_means, post_scale, df, ybar)
  names(doutp) = c("Group", "PosteriorP", "Post_mean", "Post_scale", "df", "ybar")
  row.names(doutp) = doutp$Group
  return(doutp)
}

# Define prior parameters and compute posterior probabilities assuming unknown variance
premu = mu0  # Prior mean
presize = 1  # Prior sample size equivalent (informative if larger)
p3 = 1   # Shape parameter for Gamma prior (affects variance estimation)
p4 = 50^2  # Scale parameter for Gamma prior (prior variance)

postP.Tfun(tdata, premu, presize, p3, p4)
